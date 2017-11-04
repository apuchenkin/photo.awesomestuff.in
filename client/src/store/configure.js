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

export default async function configureStore(historyProtocol, initialState) {
  const { runtime: { basename } } = initialState;

  return createStore(
    reducers,
    initialState,
    compose(
      createHistoryEnhancer({
        protocol: historyProtocol,
        middlewares: [
          queryMiddleware,
          basename && createBasenameMiddleware({ basename }),
        ].filter(Boolean),
      }),
      createMatchEnhancer(
        new Matcher(routeConfig(initialState)),
      ),
    ),
  );
}
