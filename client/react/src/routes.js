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
import Loader from './components/loader';

const categoryService = new CategoryService();
const photoService = new PhotoService();
const pageService = new PageService();

const isBrowser = (typeof window !== 'undefined');
const initialState = isBrowser && window.__INITIAL_STATE__ || [];

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
    state: initialState[2] ? initialState[2].state : {},

    getComponent(location, cb) {
      var me = this;

      me.resolve(location.params, location.routes[0].cmp).then(data => {
        me.props = Object.assign(props, data, location.routes[1].state);
        cb(null, Photo);
      })
    },

    resolve(params) {
      return utils.fetchAll({
        photo: photoService.fetchPhoto(params.photoId)
      })
    }
  })
}

const categryRoute = (category, props) => {
  const path = category.parent ? category.parent.name + '/' + category.name : category.name;

  return new CachedRoute ({
    path: path,
    state: initialState[1] && initialState[1].path === path ? initialState[1].state : {},

    resolve(params) {
      return utils.fetchAll({
        photos: photoService
          .fetchPhotos(category.name)
          .then(p => photoService.refinePhotos(p, params.photoId))
          .then(photoService.remapPhotos.bind(photoService))
      })
    },

    getComponents(location, cb) {
      var me = this,
          callback = (data) => {
            me.props = Object.assign({category: category}, props, data);
            me.components = {
              header: GalleryHeader,
              body: Gallery
            };

            cb(null, me.components);
          }
        ;

    Object.keys(me.state).length
      ? callback(me.state)
      : me.resolve(location.params, location.routes[0].cmp).then(callback)
    },

    childRoutes: [
      photoRoute({
        category: category
      })
    ]
  });
}

const pageRoute = (page) => new CachedRoute({
  path: page.alias,
  state: initialState[1] && initialState[1].path === page.alias ? initialState[1].state : {},
  resolve() {
    return utils.fetchAll({
      page: pageService.fetchPage(page.id)
    });
  },

  getComponents(location, cb) {
    var me = this,
        callback = (data) => {
          me.props = {
            page: page,
            content: data.page.content
          };
          me.components = {
            header: PageHeader,
            body: Page
          };

          cb(null, me.components);
        }
      ;

  Object.keys(me.state).length
    ? callback(me.state)
    : me.resolve(location.params, location.routes[0].cmp).then(callback)
  }
})

class CachedRoute extends Object {
  constructor(obj) {
    super(obj);

    const
      me = this,
      wrappedResolve = (promise) => {
        return (params, cmp) => {
          cmp && cmp.setState({isLoading: true});
          return promise(params)
            .then(data => {
              Object.assign(me.state, data);
              cmp && cmp.setState({isLoading: false});
              return data;
            })
        }
      }

    Object.assign(me, {
      resolve: wrappedResolve(obj.resolve)
    })
  }
}

const mainRoute = new CachedRoute({
  path: '/',
  component: Main,
  state: initialState[0] ? initialState[0].state : {},

  resolve() {
    var me = this;

    return utils.fetchAll({
      categories: categoryService.fetchCategories(),
      pages: pageService.fetchPages()
    })
  },

  getIndexRoute(location, cb) {
    const
      me = this,
      callback = (data) => {
        me.indexRoute = {
            class: 'main'
          , components: {header: HomeHeader, body: Home}
          , props: data
        };

        cb(null, me.indexRoute);
      }
    ;

    Object.keys(me.state).length
      ? callback(me.state)
      : me.resolve().then(callback);
  },

  getChildRoutes(location, cb) {
    const
      me = this,
      callback = (data) => {
        me.childRoutes = [].concat(
        data.categories.filter(c => !!c.title).map(c => categryRoute(c, data))
      , data.pages.filter(p => !!p.title).map(p => pageRoute(p))
      )

      cb(null, me.childRoutes);
    };

    Object.keys(me.state).length
      ? callback(me.state)
      : me.resolve().then(callback);
  }
});

export default mainRoute;
