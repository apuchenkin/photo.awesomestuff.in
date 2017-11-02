import Redirect from 'found/lib/Redirect';
import HttpError from 'found/lib/HttpError';
import { remapPhotos, refinePhotos } from 'common/service/api/Photo';

import {
  Home,
  Page,
  Category,
  Photo,
} from '../page';

export default (pages = [], categories = [], data = {}) => [
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
      getData: async ({ context: { services: { pageService } } }) => {
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
      getData: async ({ params, context: { store, services: { categoryService } } }) => {
        const { config } = store.getState().runtime;
        const cachedPhotos = data.category && data.category.id === category.id && data.photos;
        const photos = cachedPhotos || await categoryService.fetchPhotos(category)
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
        getData: async ({ params, context: { services: { photoService } } }) => {
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
