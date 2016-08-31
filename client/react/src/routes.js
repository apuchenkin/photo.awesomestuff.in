import React from 'react';
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
import Link from 'react-router/lib/Link';
import Main from './components/main';
import utils from './lib/utils';
import Loader from './components/loader';
import Route from './lib/route';

const
  isBrowser = (typeof window !== 'undefined'),
  initialState = isBrowser && window.__INITIAL_STATE__ || {},
  routerState = initialState.routes || [];

// class NoMatch extends React.Component {
//   render() {
//     return (
//       <div>NoMatch</div>
//     );
//   }
// }

export default (locale) => {
  const
    categoryService = new CategoryService({locale}),
    photoService = new PhotoService({locale}),
    pageService = new PageService({locale})
    ;

  const photoRoute = (props) => {
    return new Route({
      path: "photo/:photoId",
      state: routerState[2] ? routerState[2].state : {},

      getComponent(location, cb) {
        const me = this;

        me.resolve(location.params, location.routes[0].cmp).then(data => {
          me.props = Object.assign(props, data, location.routes[1].state);
          cb(null, Photo);
        });
      },

      resolve(params) {
        return utils.fetchAll({
          photo: photoService.fetchPhoto(params.photoId)
        });
      }
    });
  };

  const categryRoute = (category, props) => {
    const path = category.parent ? category.parent.name + '/' + category.name : category.name;

    return new Route({
      path,
      state: routerState[1] && routerState[1].path === path ? routerState[1].state : {},

      resolve(params) {
        return utils.fetchAll({
          photos: photoService
              .fetchPhotos(category.name)
              .then(p => photoService.refinePhotos(p, params.photoId))
              .then(photoService.remapPhotos.bind(photoService))
        });
      },

      getComponents(location, cb) {
        const
          me = this,
          callback = (data) => {
            me.props = Object.assign({category}, props, data);
            me.components = {
              header: GalleryHeader,
              body: Gallery
            };

            cb(null, me.components);
          };

        Object.keys(me.state).length
          ? callback(me.state)
          : me.resolve(location.params, location.routes[0].cmp).then(callback);
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

    getComponents(location, cb) {
      const
        me = this,
        callback = (data) => {
          me.props = {
            page,
            content: data.page.content
          };
          me.components = {
            header: PageHeader,
            body: Page
          };

          cb(null, me.components);
        };

      Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve(location.params, location.routes[0].cmp).then(callback);
    }
  });

  return new Route({
    path: "/",
    state: routerState[0] ? routerState[0].state : {},

    resolve() {
      const me = this;

      return utils.fetchAll({
        categories: categoryService.fetchCategories(),
        pages: pageService.fetchPages()
      });
    },

    getComponent(location, cb) {
      const me = this,
        callback = (data) => {
          me.props = data;
          me.component = Main;
          cb(null, me.component);
        }
      ;

      Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve(location.params, location.routes[0].cmp).then(callback);
    },

    getIndexRoute(location, cb) {
      const
        me = this,
        callback = (data) => {
          me.indexRoute = {
            class: 'main',
            components: {header: HomeHeader, body: Home},
            props: data
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
        );

          cb(null, me.childRoutes);
        };

      Object.keys(me.state).length
        ? callback(me.state)
        : me.resolve().then(callback);
    }
  });
};
