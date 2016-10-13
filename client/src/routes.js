import React from 'react';
import IntlMessageFormat from 'intl-messageformat';

import PhotoService from '../lib/service/Photo';
import Route from './lib/route';
import Home from './components/home';
import HomeHeader from './components/home/header';
import Gallery from './components/gallery';
import GalleryHeader from './components/gallery/header';
import Page from './components/page';
import PageHeader from './components/page/header';
import Photo from './components/photo';
import Main from './components/main';
import Error404 from './components/error/404';
import { startLoading } from './actions/loader';
import createActions from './actions/api';

const notFound = {
  path: '*',
  class: 'not-found',
  components: {
    header: props => <HomeHeader {...props} />,
    body: props => <Error404 {...props} />,
  },

  getMeta() {
    return {
      title: '404',
    };
  },
};

export default (store) => {
  const
    { messages } = store.getState().runtime,
    { getCategories, getCategory, getPages, getPage, getPhotos, getPhoto } = createActions(store);

  const photoRoute = new Route({
    store,
    path: 'photo/:photoId',
    component: Photo,
    actions: [
      location => getPhoto(location.params.photoId),
    ],

    onEnter(location, replace, cb) {
      const { category } = store.getState().api;

      if (!isBrowser) {
        store.dispatch(startLoading());
      }

      this.resolve(location)
        .catch(() => replace(category.parent ? `/${category.parent.name}/${category.name}` : `/${category.name}`))
        .then(() => cb());
    },

    getMeta() {
      const
        { photo } = store.getState().api,
        description = new IntlMessageFormat(messages['meta.description.photo']);

      return photo && {
        title: photo.caption,
        description: description.format({
          author: photo.author && photo.author.name,
          title: photo.caption,
        }),
      };
    },

    getLangs() {
      return store.getState().api.photo.langs;
    },
  });

  const categoryRoute = category => new Route({
    store,
    path: category.parent ? `${category.parent.name}/${category.name}` : category.name,
    actions: [
      getCategory.bind(undefined, category.name),
      (location) => {
        const action = getPhotos(category.name);

        return action && Object.assign({}, action, {
          payload: action.payload
            .then(photos => PhotoService.refinePhotos(photos, location.params.photoId))
            .then(PhotoService.remapPhotos),
        });
      },
    ],

    onEnter(location, replace, cb) {
      if (!isBrowser) {
        store.dispatch(startLoading());
      }

      // TODO: call parent
      this.resolve(location)
        .catch(cb)
        .then(() => cb());
    },

    components: {
      header: GalleryHeader,
      body: Gallery,
    },

    childRoutes: [
      photoRoute,
    ],

    getMeta() {
      const category$ = store.getState().api.category;

      return {
        title: category$.title,
        description: category$.description || category$.short_description,
      };
    },

    getLangs() {
      return store.getState().api.category.langs;
    },
  });

  const categoryRedirect = category => ({
    path: category.name,
    onEnter(location, replace) {
      replace(`/${category.parent.name}/${category.name}`);
    },
  });

  const pageRoute = page => new Route({
    store,
    path: page.alias,
    actions: [getPage.bind(undefined, page.id)],
    components: {
      header: PageHeader,
      body: Page,
    },

    getMeta() {
      return {
        title: page.title,
        description: page.description, // TODO: create page description on BE
      };
    },

    getLangs() {
      return store.getState().api.page.langs;
    },
  });

  const mainRoute = new Route({
    store,
    path: '/',
    actions: [getCategories, getPages],
    component: Main,

    indexRoute: {
      class: 'home',
      components: {
        header: HomeHeader,
        body: Home,
      },
    },

    getChildRoutes(location, cb) {
      this.resolve(location)
        .catch(cb)
        .then(() => {
          const { categories, pages } = store.getState().api;

          this.childRoutes = [].concat(
            categories.map(categoryRoute),
            categories.filter(c => !!c.parent).map(categoryRedirect),
            pages.map(pageRoute),
            [notFound]
          );

          cb(null, this.childRoutes);
        });
    },
  });

  return mainRoute;
};
