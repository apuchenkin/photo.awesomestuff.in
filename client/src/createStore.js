import { createStore, applyMiddleware, compose } from 'redux';
import promiseMiddleware from 'redux-promise';

import reducers from './reducers';

export default function configureStore(initialState) {
  let store;

  if (__DEV__) {
    if (isBrowser) {
      // eslint-disable-next-line
      window.perf = require('react-addons-perf');
    }
    // eslint-disable-next-line
    const createLogger = require('redux-logger');

    const logger = isBrowser
      ? createLogger()
      : () => next => (action) => {
        console.log(` * ${action.type}`); // eslint-disable-line no-console
        return next(action);
      };

    // https://github.com/zalmoxisus/redux-devtools-extension#redux-devtools-extension
    let devToolsExtension = f => f;
    if (process.env.BROWSER && window.devToolsExtension) {
      devToolsExtension = window.devToolsExtension();
    }

    const enhancer = compose(
      applyMiddleware(...[promiseMiddleware, logger]),
      devToolsExtension,
    );

    store = createStore(reducers, initialState, enhancer);
  } else {
    store = createStore(reducers, initialState, applyMiddleware(promiseMiddleware));
  }

  return store;
}
