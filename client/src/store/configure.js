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
  const { data = [], ...initial } = initialState;

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
        new Matcher(routeConfig(pages, categories, data[1])),
      ),
    ),
  );
}
