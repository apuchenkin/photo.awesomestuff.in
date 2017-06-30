import React from 'react';
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

const metaUpdate = (meta) => {
  document.title = meta.title;
  document.head.querySelector('meta[name=description]').content = meta.description;
  Array.from(document.head.querySelectorAll('link[hreflang]')).map((node) => {
    ReactDOM.unmountComponentAtNode(node);
    document.head.removeChild(node);
    return false;
  });
  const links = meta.links.reduce((acc, link) => {
    ReactDOM.render(link, span);
    return acc.concat(span.innerHTML);
  }, []);
  document.head.insertAdjacentHTML('beforeend', links.join('\n'));

  // if (typeof ga !== 'undefined') {
  //   ga('send', 'pageview', {
  //     title: meta.title,
  //     page: location.pathname,
  //   });
  // }

  //TODO: pass meta to server
};

const render = ({ Component, props, data }) => {
  metaUpdate(data.meta);
  return <Component {...props} />;
};

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
        render,
        getData: ({ routes, context: { store } }) => {
          store.dispatch(setRuntimeVariable('langs', null));

          console.log(1);
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
        render,
        getData: async ({ context: { store } }) => {
          const page$ = await new Promise((resolve, reject) => {
            store.dispatch(loadPage(page, resolve, reject));
          });
          console.log(2);
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
        render,
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
          render,
          getData: async ({ params, context: { store } }) => (
            new Promise((resolve, reject) => {
              store.dispatch(loadPhoto(params.photoId, resolve, reject));
            })).catch(() => {
              throw new HttpError(404);
            }),
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
