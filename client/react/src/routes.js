import React from 'react';
import Link from 'react-router/lib/Link';
import Redirect from 'react-router/lib/Redirect';

import CategoryService from './service/Category';
import PhotoService from './service/Photo';
import PageService from './service/Page';

import Home from './components/home';
import HomeHeader from './components/home/header';
import Gallery from './components/gallery';
import GalleryHeader from './components/gallery/header';
import Page from './components/page';
import PageHeader from './components/page/header';
import Photo from './components/photo';
import Main from './components/main';
import utils from './lib/utils';
import Loader from './components/loader';
import Route from './lib/route';
import Error404 from './components/error/404';

const
  isBrowser = (typeof window !== 'undefined'),
  initialState = isBrowser && window.__INITIAL_STATE__ || {},
  routerState = initialState.routes || [];

//TODO: use sate instead of props
export default (locale) => {
  const
    categoryService = new CategoryService({locale}),
    photoService = new PhotoService({locale}),
    pageService = new PageService({locale})
    ;

  const photoRoute = (data) => {
    return new Route({
      path: "photo/:photoId",
      state: routerState[2] ? routerState[2].state : {},

      onEnter(location, replace, cb) {
        const me = this;
        me.resolve(location)
          .then(() => {
            Object.assign(me, {component: props => <Photo {...props} photo={me.state.photo} category={data.category} />});
            cb();
          })
          .catch((e) => {
            replace("/"); //TODO: reaplce to catregory
            cb();
          });
      },

      resolve(location) {
        return utils.fetchAll({
          photo: photoService.fetchPhoto(location.params.photoId)
        });
      }
    });
  };

  const categoryRoute = (category, data) => {
    const path = category.parent ? category.parent.name + '/' + category.name : category.name;

    return new Route({
      path,
      state: (routerState[1] && routerState[1].path === path) ? routerState[1].state : {},

      resolve(location) {
        return utils.fetchAll({
          photos: photoService
            .fetchPhotos(category.name)
            .then(p => photoService.refinePhotos(p, location.params.photoId))
            .then(photoService.remapPhotos.bind(photoService))
        });
      },

      onEnter(location, replace, cb) {
        const me = this;
        const callback = () => {
          Object.assign(me, {components: {
            header: props => <GalleryHeader {...props} category={category} categories={data.categories} />,
            body: props => <Gallery {...props} category={category} photos={me.state.photos} />
          }});
          cb();
        };

        Object.keys(me.state).length
          ? callback()
          : me.resolve(location)
              .then(callback)
              .catch(cb);
      },

      childRoutes: [
        photoRoute({
          category
        })
      ]
    });
  };

  const pageRoute = (page) => new Route({
    path: page.alias,
    state: routerState[1] && routerState[1].path === page.alias ? routerState[1].state : {},

    resolve() {
      return utils.fetchAll({
        page: pageService.fetchPage(page.id)
      });
    },

    onEnter(location, replace, cb) {
      const me = this;
      const callback = () => {
        Object.assign(me, {components: {
          header: props => <PageHeader {...props} page={me.state.page} />,
          body: props => <Page {...props} content={me.state.page.content} />
        }});
        cb();
      };

      Object.keys(me.state).length
        ? callback()
        : me.resolve(location)
            .then(callback)
            .catch(cb);
    }
  });

  const mainRoute = new Route({
    path: "/",
    state: routerState[0] ? routerState[0].state : {},

    resolve() {
      const me = this;

      return utils.fetchAll({
        categories: categoryService.fetchCategories(),
        pages: pageService.fetchPages()
      });
    },

    onEnter(location, replace, cb) {
      const me = this;
      const callback = () => {
        Object.assign(me, {component: props => <Main {...props} pages={this.state.pages} />});
        cb();
      };

      Object.keys(this.state).length
        ? callback()
        : this.resolve(location)
            .then(callback)
            .catch(cb);
    },

    getIndexRoute(location, cb) {
      console.log('getIndexRoute');
      const
        me = this,
        callback = (data) => {
          me.indexRoute = {
            class: 'main',
            components: {
              header: props => <HomeHeader {...props} />,
              body: props => <Home {...props} categories={me.state.categories} />
            }
          };

          cb(null, me.indexRoute);
        }
        ;

      Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve(location).then(callback);
    },

    getChildRoutes(location, cb) {
      console.log('getChildRoutes');
      const
        me = this,
        callback = (data) => {
          me.childRoutes = [notFound].concat(
            data.categories.filter(c => !!c.title).map(c => categoryRoute(c, data))
            , data.pages.filter(p => !!p.title).map(p => pageRoute(p))
          );

          cb(null, me.childRoutes);
        };

      Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve(location).then(callback);
    }
  });

  const redirect404 = {path: '*', onEnter: (nextState, replace) => replace('/404')};

  const notFound = new Route({
    path: "404",
    components: {
      header: HomeHeader,
      body: Error404
    }
  });

  return [
    mainRoute,
    redirect404
  ];
};
