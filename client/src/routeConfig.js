import { HttpError, RedirectException } from 'found';
import Main from './components/main';
import HomeHeader from './components/home/header';
import PageHeader from './components/page/header';
import GalleryHeader from './components/gallery/header';
import Home from './components/home';
import Page from './components/page';
import Gallery from './components/gallery';

import {
  loadAll as loadCategories,
  load as loadCategory,
} from './store/category/actions';
import {
  loadAll as loadPhotos,
  // load as loadPhoto,
} from './store/photo/actions';
import { load as loadPage } from './store/page/actions';

export default pages => [
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
        getData: async ({ routes, context: { store } }) => {
          await new Promise((resolve, reject) => {
            store.dispatch(loadCategories(resolve, reject));
          });

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

          return {
            meta: {
              title: page$.title,
              description: page$.description,
            },
            langs: page$.langs,
          };
        },
      })),
      {
        path: '/:category/:subcategory?',
        header: GalleryHeader,
        getData: async ({ params, context: { store } }) => {
          const category = await (new Promise((resolve, reject) => {
            store.dispatch(loadCategory(params.subcategory || params.category, resolve, reject));
          })).catch(() => {
            throw new HttpError(404);
          });

          if (!params.subcategory && category.parent) {
            throw new RedirectException(`/${category.parent.name}/${category.name}`);
          }

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
            langs: category.langs,
          };
        },
        Component: Gallery,
      },
    ],
  },
];
