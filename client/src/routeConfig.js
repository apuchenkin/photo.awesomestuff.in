import { HttpError, Redirect } from 'found';
import Main from './components/main';
import HomeHeader from './components/home/header';
import PageHeader from './components/page/header';
import GalleryHeader from './components/gallery/header';
import Home from './components/home';
import Page from './components/page';
import Gallery from './components/gallery';
import Photo from './components/photo';

import {
  loaded as loadedCategory,
} from './store/category/actions';

import {
  loadAll as loadPhotos,
  load as loadPhoto,
} from './store/photo/actions';

import { load as loadPage } from './store/page/actions';
import { setMeta } from './store/meta/actions';

export default (pages, categories) => [
  {
    path: '/',
    Component: Main,
    getData: ({ routes }) => {
      const route = routes[routes.length - 1];

      return {
        header: route.header,
        className: route.className,
      };
    },
    children: [
      {
        header: HomeHeader,
        Component: Home,
        className: 'home',
        getData: ({ context: { store } }) => {
          const { config } = store.getState().runtime;
          store.dispatch(setMeta({
            langs: config.locales,
            title: null,
            description: null,
          }));
        },
      },
      ...pages.map(page => ({
        path: page.alias,
        header: PageHeader,
        Component: Page,
        getData: async ({ context: { store } }) => {
          const page$ = await new Promise((resolve, reject) => {
            store.dispatch(loadPage(page, resolve, reject));
          });

          store.dispatch(setMeta({
            langs: page$.langs,
            title: page$.title,
            description: page$.description,
          }));

          return page$;
        },
      })),
      ...categories.map(category => ({
        path: category.parent
          ? `/${category.parent.name}/${category.name}`
          : `/${category.name}`,
        header: GalleryHeader,
        Component: Gallery,
        getData: async ({ context: { store } }) => {
          store.dispatch(loadedCategory(category));

          await (new Promise((resolve, reject) => {
            store.dispatch(loadPhotos(category, resolve, reject));
          })).catch(() => {
            throw new HttpError(404);
          });

          store.dispatch(setMeta({
            langs: category.langs,
            title: category.title,
            description: category.description,
          }));

          return category;
        },
        children: [{
          path: '/photo/:photoId',
          header: GalleryHeader,
          Component: Photo,
          getData: async ({ params, context: { store, intl } }) => {
            const photo = await new Promise((resolve, reject) => {
              store.dispatch(loadPhoto(params.photoId, resolve, reject));
            }).catch(() => {
              throw new HttpError(404);
            });

            store.dispatch(setMeta({
              langs: photo.langs,
              title: photo.description,
              description: intl.formatMessage({ id: 'meta.description.photo' }, {
                author: photo.author && photo.author.name,
                title: photo.description,
              }),
            }));

            return photo;
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
