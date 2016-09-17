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

const
  isBrowser = (typeof window !== 'undefined'),
  initialState = isBrowser ? window.__INITIAL_STATE__ || {} : {},
  routerState = initialState.routes || [];

const notFound = new Route({
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
});

export default (locale, messages = initialState.messages) => {
  const
    categoryService = new CategoryService({ locale }),
    photoService = new PhotoService({ locale }),
    pageService = new PageService({ locale })
    ;

  const photoRoute = category => new Route({
    path: 'photo/:photoId',
    state: routerState[2] ? routerState[2].state : {},

    onEnter(location, replace, cb) {
      const me = this;

      me.resolve(location)
        .then(() => Object.assign(me, {
          component: props => <Photo {...props} photo={me.state.photo} category={category} />,
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
      state: (routerState[1] && routerState[1].path === path) ? routerState[1].state : {},

      resolve(location) {
        return utils.fetchAll({
          category: categoryService.fetchCategory(category.name),
          photos: photoService
            .fetchPhotos(category.name)
            .then(p => photoService.refinePhotos(p, location.params.photoId))
            .then(photoService.remapPhotos.bind(photoService)),
        });
      },

      onEnter(location, replace, cb) {
        const me = this;
        const callback = () => {
          Object.assign(me, { components: {
            header: props => <GalleryHeader
              {...props} category={category} categories={data.categories}
            />,
            body: props => <Gallery {...props} category={category} photos={me.state.photos} />,
          } });
          cb();
        };

        return Object.keys(me.state).length
          ? callback()
          : me.resolve(location)
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
    path: '/',
    state: routerState[0] ? routerState[0].state : {},

    resolve() {
      return utils.fetchAll({
        categories: categoryService.fetchCategories(),
        pages: pageService.fetchPages(),
      });
    },

    onEnter(location, replace, cb) {
      const me = this;
      const callback = () => {
        Object.assign(me, { component: props => <Main {...props} pages={this.state.pages} /> });
        cb();
      };

      return Object.keys(this.state).length
        ? callback()
        : this.resolve(location)
            .then(callback)
            .catch(cb);
    },

    getIndexRoute(location, cb) {
      const
        me = this,
        callback = () => {
          me.indexRoute = {
            class: 'main',
            components: {
              header: props => <HomeHeader {...props} />,
              body: props => <Home {...props} categories={me.state.categories} />,
            },
          };

          cb(null, me.indexRoute);
        }
        ;

      return Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve(location)
            .then(callback)
            .catch(cb);
    },

    getChildRoutes(location, cb) {
      const
        me = this,
        callback = (data) => {
          me.childRoutes = [].concat(
            data.categories.filter(c => !!c.title).map(c => categoryRoute(c, data)),
            data.categories.filter(c => !!c.title && !!c.parent).map(categoryRedirect),
            data.pages.filter(p => !!p.title).map(pageRoute),
            [notFound]
          );

          cb(null, me.childRoutes);
        };

      return Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve(location).then(callback);
    },
  });

  return mainRoute;
};
