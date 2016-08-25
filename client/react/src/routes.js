import React from 'react';
import { Route, Redirect, createRoutes, withRouter, IndexRoute } from 'react-router';
import CategoryService from './service/Category';
import PhotoService from './service/Photo';
import PageService from './service/Page';
import Home from './components/home';
import HomeHeader from './components/home/header';
import Gallery from './components/gallery';
import GalleryHeader from './components/gallery/header';
import Page from './components/page';
import PageHeader from './components/page/header';
import Promise from 'promise';
import Photo from './components/photo';
import Link from 'react-router/lib/Link';
import Main from './components/main';
import utils from './lib/utils';

const categoryService = new CategoryService();
const photoService = new PhotoService();
const pageService = new PageService();

const isBrowser = (typeof window !== 'undefined');
const initialState = isBrowser && window.__INITIAL_STATE__ || {};

class NoMatch extends React.Component {
  render() {
    return (
      <div>NoMatch</div>
    )
  }
}

const photoRoute = (props) => {
  return new CachedRoute ({
    path: "photo/:photoId",

    getComponent(location, cb) {
      var me = this;
      me.resolve(location.params).then(data => {
        me.props = Object.assign(props, data);
        cb(null, Photo);
      })
    },

    resolve(params) {
      return utils.fetchAll({
        photo: photoService.fetchPhoto(params.photoId)
      })
    }
  }, utils.pick(initialState, ['photo']))
}

const categryRoute = (category, props) => {
  const path = category.parent ? category.parent.name + '/' + category.name : category.name;

  return new CachedRoute ({
    path: path,
    resolve(params) {
      return utils.fetchAll({
        photos: photoService.fetchPhotos(category.name).then(p =>
          photoService.remapPhotos(
            photoService.refinePhotos(p, params.photoId)
          )
        )
      })
    },

    getComponents(location, cb) {
      var me = this;
      me.resolve(location.params).then(data => {
        me.props = Object.assign({category: category}, props, data);
        cb(null, {
          header: GalleryHeader,
          body: Gallery
        });
      })
    },

    getChildRoutes(location, cb) {
      var me = this;
      me.resolve(location.params).then(data => {
        cb(null, [
          photoRoute(Object.assign({
            category: category,
            id: location.params.photoId
          }, data))
        ]);
      })
    }
  }, utils.pick(initialState, ['photos']));
}

const pageRoute = (page) => {
  return page.title && new CachedRoute({
    path: page.alias,
    resolve() {
      return utils.fetchAll({
        page: pageService.fetchPage(page.id)
      });
    },

    getComponents(location, cb) {
      var me = this;
      me.resolve().then(data => {
        me.props = {
          page: page,
          content: data.page.content
        };

        cb(null, {
          header: PageHeader,
          body: Page
        });
      })
    }
  }, utils.pick(initialState, ['page']))
}

class CachedRoute extends Object {
  constructor(obj, cache) {
    super(obj);
    const
      me = this,
      resolve = obj.resolve;

    me.cache = cache;
    me.parent = {
      resolve: resolve
    };

    Object.assign(me, {
      resolve(params) {
        return Object.keys(me.cache).length
          ? Promise.resolve(me.cache)
          : resolve(params)
            .then(data => Object.assign(me.cache, data))
      }
    })
  }
}

const mainRoute = new CachedRoute({
  path: '/',
  component: Main,

  resolve() {
    return utils.fetchAll({
      categories: categoryService.fetchCategories(),
      pages: pageService.fetchPages()
    })
  },

  getIndexRoute(location, cb) {
    var me = this;
    me.resolve().then(data => {
      cb(null, {
        components: {header: HomeHeader, body: Home},
        props: data
      });
    })
  },

  getChildRoutes(location, cb) {
    var me = this;
    me.resolve().then(data => {
      cb(null, [].concat(
          data.categories.map(c => categryRoute(c, data)),
          data.pages.map(p => pageRoute(p))
        ));
    })
  }
}, initialState);

export default mainRoute;
