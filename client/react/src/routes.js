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
    state: utils.pick(initialState, ['photo']),

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
  })
}

const categryRoute = (category, props) => {
  const path = category.parent ? category.parent.name + '/' + category.name : category.name;

  return new CachedRoute ({
    path: path,
    state: utils.pick(initialState, ['photos']),
    // class: !!(category.parent || category).childs.length ? 'nav' : '',
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
  });
}

const pageRoute = (page) => {
  return page.title && new CachedRoute({
    path: page.alias,
    state: utils.pick(initialState, ['page']),
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
  })
}

class CachedRoute extends Object {
  constructor(obj) {
    super(obj);

    const
      me = this,
      wrappedResolve = (promise) => {
        return (params) => {
          const cmp = me.cmp;
          console.log('cmp', cmp);
          cmp && cmp.setState({isLoading: true});
          return promise(params)
            .then(data => {
              Object.assign(me.state, data);
              console.log('stopLoading');
              cmp && cmp.setState({isLoading: false});
              return data;
            })
        }
      }

    Object.assign(me, {
      resolve: wrappedResolve(obj.resolve),

      //TODO: check memory leaks while binding cmp to route
      connect: (cmp) => {
        this.cmp = cmp;
      },
    })
  }
}

const mainRoute = new CachedRoute({
  path: '/',
  component: Main,
  state: utils.pick(initialState, ['categories', 'pages']),

  resolve() {
    var me = this;

    return utils.fetchAll({
      categories: categoryService.fetchCategories(),
      pages: pageService.fetchPages()
    })
  },

  // onChange(prevState){
  //   console.log("onChange", arguments);
  // },
  //
  // onEnter(nextState) {
  //   console.log("onEnter");
  // },

  getIndexRoute(location, cb) {
    const
      me = this,
      callback = (data) => cb(null, {
        class: 'main'
      , components: {header: HomeHeader, body: Home}
      , props: data
      })
    ;

    console.log('getIndexRoute', me.state);
    Object.keys(me.state).length
      ? callback(me.state)
      : me.resolve().then(callback);
  },

  getChildRoutes(location, cb) {
    const
      me = this,
      callback = (data) => cb(null, [].concat(
        data.categories.map(c => categryRoute(c, data))
      , data.pages.map(p => pageRoute(p))
      ))
    ;

    console.log('getChildRoutes', me.state);
    Object.keys(me.state).length
      ? callback(me.state)
      : me.resolve().then(callback);
  }
});

export default mainRoute;
