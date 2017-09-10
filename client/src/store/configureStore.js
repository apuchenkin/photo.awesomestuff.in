import { createStore, compose } from 'redux';

import CategoryService from 'common/service/api/Category';
import PageService from 'common/service/api/Page';
import PhotoService from 'common/service/api/Photo';

import {
  createHistoryEnhancer,
  queryMiddleware,
  createBasenameMiddleware,
} from 'farce';
import createMatchEnhancer from 'found/lib/createMatchEnhancer';
import Matcher from 'found/lib/Matcher';

import reducers from './reducer';
import routeConfig from '../routeConfig';


export default async function configureStore(historyProtocol, initialState, intl) {
  const { runtime: { locale, basename, config: { apiEndpoint } }, page, category } = initialState;
  const defaults = { locale, apiEndpoint };

  const categoryService = new CategoryService(defaults);
  const pageService = new PageService(defaults);
  const photoService = new PhotoService(defaults);

  const pages = (page && page.pages)
    || await pageService.fetchPages().catch(() => []);

  const categories = (category && category.categories)
    || await categoryService.fetchCategories().catch(() => []);

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
