import { createStore, compose } from 'redux';

import {
  createHistoryEnhancer,
  queryMiddleware,
  createBasenameMiddleware,
} from 'farce';
import createMatchEnhancer from 'found/lib/createMatchEnhancer';
import Matcher from 'found/lib/Matcher';

import reducers from './reducer';
import routeConfig from '../router/config';

// const defaults = { locale, apiEndpoint };

export default async function configureStore(historyProtocol, initialState) {
  const { runtime: { basename }, page: { pages }, category: { categories } } = initialState;
  // const {
  //   pageService,
  //   categoryService,
  // } = services;


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
    ),
  );
}
