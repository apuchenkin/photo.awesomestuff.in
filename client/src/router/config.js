import Redirect from 'found/lib/Redirect';
import HttpError from 'found/lib/HttpError';
import { remapPhotos, refinePhotos } from 'common/service/api/Photo';

import {
  Home,
  Page,
  Category,
  Photo,
} from '../page';

import {
  selectors,
} from '../store/cache';

export default ({ page: { pages }, category: { categories } }) => [
  {
    path: '/',
    children: [{
      Component: Home,
      data: ({
        pages,
        categories,
      }),
    },
    ...pages.map(page$ => ({
      path: page$.alias,
      Component: Page,
      getData: async ({ routeIndices, context: { store, services: { pageService } } }) => {
        const { cache: { cache } } = store.getState();
        const routeCache = selectors.getCache(cache, routeIndices.slice(0, 1));

        if (routeCache) {
          return routeCache;
        }

        const page = await pageService
          .fetchPage(page$)
          .catch(({ error }) => {
            throw new HttpError(404, error);
          });

        return {
          page,
        };
      },
    })),
    ...categories.map(category => ({
      path: category.parent
        ? `/${category.parent.name}/${category.name}`
        : `/${category.name}`,
      Component: Category,
      getData: async ({
        routeIndices,
        params,
        context: { store, services: { categoryService } },
      }) => {
        const { runtime: { config }, cache: { cache } } = store.getState();
        const routeCache = selectors.getCache(cache, routeIndices.slice(0, 2));

        if (routeCache) {
          return routeCache;
        }

        const photos = await categoryService.fetchPhotos(category)
          .then(refinePhotos(params.photoId))
          .then(remapPhotos({ width: config.brickWidth, gutter: config.gutter }));

        return {
          category,
          categories,
          photos,
          config,
        };
      },
      children: [{
        path: '/photo/:photoId',
        Component: Photo,
        getData: async ({
          routeIndices,
          params,
          context: { store, services: { photoService } },
        }) => {
          const { cache: { cache } } = store.getState();
          const routeCache = selectors.getCache(cache, routeIndices);

          if (routeCache) {
            return routeCache;
          }

          const photo = await photoService
            .fetchPhoto(params.photoId)
            .catch(({ error }) => {
              throw new HttpError(404, error);
            });

          return {
            photo,
          };
        },
      }],
    })),
    ...categories
      .filter(category => category.parent)
      .map(category => new Redirect({
        from: category.name,
        to: `/${category.parent.name}/${category.name}`,
      })),
    ],
  },
];
