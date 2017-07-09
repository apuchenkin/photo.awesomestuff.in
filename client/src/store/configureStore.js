import { createStore, applyMiddleware, compose } from 'redux';
import { createEpicMiddleware } from 'redux-observable';
import {
  createHistoryEnhancer,
  queryMiddleware,
  createBasenameMiddleware,
} from 'farce';
import createMatchEnhancer from 'found/lib/createMatchEnhancer';
import Matcher from 'found/lib/Matcher';

import epic from './epic';
import reducers from './reducer';
import routeConfig from '../routeConfig';

import CategoryService from '../../../common/service/api/Category';
import PageService from '../../../common/service/api/Page';
import PhotoService from '../../../common/service/api/Photo';

import actionLifecyclesMiddleware from './actionLifecyclesMiddleware';

export default async function configureStore(historyProtocol, initialState, intl) {
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

  const { runtime: { locale, basename, config: { apiEndpoint } }, page, category } = initialState;
  const defaults = { locale, apiEndpoint };

  const categoryService = new CategoryService(defaults);
  const pageService = new PageService(defaults);
  const photoService = new PhotoService(defaults);

  const pages = (page && page.pages)
    || await pageService.fetchPages().catch((e) => {
      console.log('error fetchPages', e);
      return [];
    });

  const categories = (category && category.categories)
    || await categoryService.fetchCategories().catch((e) => {
      console.log('error fetchPages', e);
      return [];
    });

    console.log(pages, categories);

  const initial = Object.assign(initialState, {
    page: { ...initialState.page, pages },
    category: { ...initialState.category, categories },
  });

  return createStore(
    reducers,
    initial,
    compose(
      createHistoryEnhancer({
        protocol: historyProtocol,
        middlewares: [
          queryMiddleware,
          basename && createBasenameMiddleware({ basename }),
        ].filter(Boolean),
      }),
      createMatchEnhancer(
        new Matcher(routeConfig(pages, categories)),
      ),
      applyMiddleware(createEpicMiddleware(epic, {
        dependencies: {
          categoryService,
          pageService,
          photoService,
          intl,
        },
      })),
      applyMiddleware(actionLifecyclesMiddleware),
    ),
  );
}
