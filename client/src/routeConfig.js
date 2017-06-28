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
import { setRuntimeVariable } from './store/runtime/actions';

export default (pages, categories) => [
  {
    path: '/',
    Component: Main,
    getData: ({ routes }) => ({
      className: 'home',
      header: routes[routes.length - 1].header,
    }),
    children: [
      {
        data: {
          className: 'home-main',
        },
        header: HomeHeader,
        Component: Home,
        getData: ({ routes, context: { store } }) => {
          store.dispatch(setRuntimeVariable('langs', null));

          return {
            className: 'home-main',
            header: routes[routes.length - 1].header,
          };
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
          // store.dispatch

          return {
            meta: {
              title: page$.title,
              description: page$.description,
            },
          };
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
          store.dispatch(setRuntimeVariable('langs', category.langs));

          await (new Promise((resolve, reject) => {
            store.dispatch(loadPhotos(category, resolve, reject));
          })).catch(() => {
            throw new HttpError(404);
          });

          return {
            meta: {
              title: category.title,
              description: category.description,
            },
          };
        },
        children: [{
          path: '/photo/:photoId',
          header: GalleryHeader,
          Component: Photo,
          getData: async ({ params, context: { store } }) => {
            const photo = await (new Promise((resolve, reject) => {
              store.dispatch(loadPhoto(params.photoId, resolve, reject));
            })).catch(() => {
              throw new HttpError(404);
            });
            // const description = new IntlMessageFormat(messages['meta.description.photo']);

            return {
              meta: {
                title: photo.caption,
                // description: description.format({
                //   author: photo.author && photo.author.name,
                //   title: photo.caption,
                // }),
              },
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
