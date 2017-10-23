// import Redirect from 'found/lib/Redirect';
import HttpError from 'found/lib/HttpError';

import {
  Main,
  Home,
  Page,
  // Category,
  // Photo
} from '../page';


import { Header as PageHeader } from '../components/page';
// import PageHeader from './components/page/header';
// import GalleryHeader from './components/gallery/header';

// import {
//   loaded as loadedCategory,
// } from './store/category/actions';

// import {
//   loadAll as loadPhotos,
//   load as loadPhoto,
// } from './store/photo/actions';

// import { load as loadPage } from './store/page/actions';
import { setMeta } from '../store/meta/actions';

export default (pages = [], categories = []) => [
  {
    path: '/',
    // Component: Main,
    // getData: ({ routes }) => ({
    //   pages,
    //   header: routes.pop().header || HomeHeader,
    // }),
    children: [{
      Component: Home,
      // header: HomeHeader,
      getData: ({ context: { store } }) => {
        const { config } = store.getState().runtime;

        store.dispatch(setMeta({
          langs: config.locales,
          title: null,
          description: null,
        }));

        return {
          categories,
        };
      },
    },
    ...pages.map(page$ => ({
      path: page$.alias,
      // header: PageHeader,
      Component: Page,
      getData: async ({ context: { store, services: { pageService } } }) => {
        const page = await pageService
          .fetchPage(page$)
          .catch(({ error }) => {
            throw new HttpError(404, error);
          });

        store.dispatch(setMeta({
          langs: page.langs,
          title: page.title,
          description: page.description,
        }));

        return {
          page,
        };
      },
    })),
    // ...categories.map(category => ({
    //   path: category.parent
    //     ? `/${category.parent.name}/${category.name}`
    //     : `/${category.name}`,
    //   header: GalleryHeader,
    //   Component: Gallery,
    //   getData: async ({ params, context: { store } }) => {
    //     store.dispatch(loadedCategory(category));
    //     await store.dispatch(loadPhotos(category, params.photoId))
    //       .catch(({ error }) => {
    //         throw new HttpError(404, error);
    //       });
    //
    //     store.dispatch(setMeta({
    //       langs: category.langs,
    //       title: category.title,
    //       description: category.description,
    //     }));
    //
    //     return category;
    //   },
    //   children: [{
    //     path: '/photo/:photoId',
    //     header: GalleryHeader,
    //     Component: Photo,
    //     getData: async ({ params, context: { store, intl } }) => {
    //       const photo = await store.dispatch(loadPhoto(params.photoId))
    //         .catch(({ error }) => {
    //           throw new HttpError(404, error);
    //         });
    //
    //       store.dispatch(setMeta({
    //         langs: photo.langs,
    //         title: photo.description,
    //         description: intl.formatMessage({ id: 'meta.description.photo' }, {
    //           author: photo.author && photo.author.name,
    //           title: photo.description,
    //         }),
    //       }));
    //
    //       return photo;
    //     },
    //   }],
    // })),
    // ...categories
    //   .filter(category => category.parent)
    //   .map(category => new Redirect({
    //     from: category.name,
    //     to: `/${category.parent.name}/${category.name}`,
    //   })),
    ],
  },
];
