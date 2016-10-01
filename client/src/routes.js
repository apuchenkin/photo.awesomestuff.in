import React from 'react';
import IntlMessageFormat from 'intl-messageformat';

import CategoryService from './service/Category';
import PhotoService from './service/Photo';
import PageService from './service/Page';

import utils from './lib/utils';
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
import { getCategories, getPages } from './actions/api';

const
  initialState = isBrowser ? window.__INITIAL_STATE__ || {} : {},
  routerState = initialState.routes || [];

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
    { locale, messages } = store.getState().runtime,
    categoryService = new CategoryService({ locale }),
    photoService = new PhotoService({ locale }),
    pageService = new PageService({ locale })
    ;

  const photoRoute = category => new Route({
    store,
    path: 'photo/:photoId',
    state: routerState[2] ? routerState[2].state : {},

    onEnter(location, replace, cb) {
      if (!isBrowser) {
        store.dispatch(startLoading());
      }

      this.resolve(location)
        .then(() => Object.assign(this, {
          component: props => <Photo {...props} photo={this.state.photo} category={category} />,
        }))
        .catch(() => replace(`/${category.parent}` ? `${category.parent.name}/${category.name}` : category.name))
        .then(() => cb());
    },

    resolve(location) {
      return utils.fetchAll({
        photo: photoService.fetchPhoto(location.params.photoId),
      });
    },

    getMeta() {
      const
        { photo } = this.state,
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
      const { photo } = this.state;
      return photo.langs;
    },
  });

  const categoryRoute = (category, data) => {
    const path = category.parent ? `${category.parent.name}/${category.name}` : category.name;

    return new Route({
      path,
      store,
      state: (routerState[1] && routerState[1].path === path) ? routerState[1].state : {},

      resolve(location) {
        if (!isBrowser) {
          store.dispatch(startLoading());
        }
        return utils.fetchAll({
          category: categoryService.fetchCategory(category.name),
          photos: photoService
            .fetchPhotos(category.name)
            .then(photos => PhotoService.refinePhotos(photos, location.params.photoId))
            .then(PhotoService.remapPhotos),
        });
      },

      onEnter(location, replace, cb) {
        const callback = () => {
          Object.assign(this, { components: {
            header: props => <GalleryHeader
              {...props} category={category} categories={data.categories}
            />,
            body: props => <Gallery {...props} category={category} photos={this.state.photos} />,
          } });
          cb();
        };

        return Object.keys(this.state).length
          ? callback()
          : this.resolve(location)
              .then(callback)
              .catch(cb);
      },

      childRoutes: [
        photoRoute(category),
      ],

      getMeta() {
        const category$ = this.state.category;

        return {
          title: category$.title,
          description: category$.description || category$.short_description,
        };
      },

      getLangs() {
        return this.state.category.langs;
      },
    });
  };

  const categoryRedirect = category => ({
    path: category.name,
    onEnter(location, replace) {
      replace(`/${category.parent.name}/${category.name}`);
    },
  });

  const pageRoute = page => new Route({
    store,
    path: page.alias,
    state: routerState[1] && routerState[1].path === page.alias ? routerState[1].state : {},

    resolve() {
      return utils.fetchAll({
        page: pageService.fetchPage(page.id),
      });
    },

    onEnter(location, replace, cb) {
      const me = this;
      const callback = () => {
        Object.assign(me, { components: {
          header: props => <PageHeader {...props} page={me.state.page} />,
          body: props => <Page {...props} content={me.state.page.content} />,
        } });
        cb();
      };

      return Object.keys(me.state).length
        ? callback()
        : me.resolve(location)
            .then(callback)
            .catch(cb);
    },

    getMeta() {
      return {
        title: page.title,
        description: page.description, // TODO: create page description on BE
      };
    },

    getLangs() {
      return this.state.page.langs;
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
      this.resolve(location).then(() => {
        const data = store.getState().api;

        this.childRoutes = [].concat(
          data.categories.filter(c => !!c.title).map(c => categoryRoute(c, data)),
          data.categories.filter(c => !!c.title && !!c.parent).map(categoryRedirect),
          data.pages.filter(p => !!p.title).map(pageRoute),
          [notFound]
        );

        cb(null, this.childRoutes);
      });
    },
  });

  return mainRoute;
};
