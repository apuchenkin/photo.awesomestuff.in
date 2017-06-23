import { createStore, applyMiddleware, compose } from 'redux';
import { createEpicMiddleware } from 'redux-observable';

import { createHistoryEnhancer, queryMiddleware } from 'farce';
import createMatchEnhancer from 'found/lib/createMatchEnhancer';
import Matcher from 'found/lib/Matcher';

import epic from './epic';
import reducers from './reducer';
import routeConfig from '../routeConfig';

import CategoryService from '../../lib/service/Category';

export default function configureStore(historyProtocol, initialState) {
  // let store;
  // if (__DEV__) {
  //   if (isBrowser) {
  //     // eslint-disable-next-line
  //     window.perf = require('react-addons-perf');
  //   }
  //   // eslint-disable-next-line
  //   const createLogger = require('redux-logger');
  //
  //   const logger = isBrowser
  //     ? createLogger()
  //     : () => next => (action) => {
  //       console.log(` * ${action.type}`); // eslint-disable-line no-console
  //       return next(action);
  //     };
  //
  //   // https://github.com/zalmoxisus/redux-devtools-extension#redux-devtools-extension
  //   let devToolsExtension = f => f;
  //   if (process.env.BROWSER && window.devToolsExtension) {
  //     devToolsExtension = window.devToolsExtension();
  //   }
  //
  //   const enhancer = compose(
  //     applyMiddleware(...[promiseMiddleware, logger]),
  //     devToolsExtension,
  //   );
  //
  //   store = createStore(reducers, initialState, enhancer);

  const { runtime: { locale, config: { apiEndpoint } } } = initialState;

  const categoryService = new CategoryService({ locale, apiEndpoint });

  return createStore(
    reducers,
    initialState,
    compose(
      createHistoryEnhancer({
        protocol: historyProtocol,
        middlewares: [queryMiddleware],
      }),
      createMatchEnhancer(
        new Matcher(routeConfig),
      ),
      applyMiddleware(createEpicMiddleware(epic, {
        dependencies: { categoryService },
      })),
    ),
  );
}
