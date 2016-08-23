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

import Photo from './components/photo';
import Link from 'react-router/lib/Link';
import Main from './components/main';
import utils from './lib/utils';

import './style/main.less';


const categoryService = new CategoryService();
const photoService = new PhotoService();
const pageService = new PageService();

class NoMatch extends React.Component {
  render() {
    return (
      <div>NoMatch</div>
    )
  }
}

const categryRoute = (category) => {
  const path = category.parent ? category.parent.name + '/' + category.name : category.name;

  return (
    <Route path={path}
      components={{header: GalleryHeader, body: Gallery}}
      resolve={params => utils.fetchAll({
    			photos: photoService.fetchPhotos(category.name).then(p =>
    				photoService.remapPhotos(
    					photoService.refinePhotos(p, params.photoId)
    				)
    			)
  		})}
      >
      <Route path="photo/:photoId"
        component={Photo}
        resolve={params => utils.fetchAll({
          photo: photoService.fetchPhoto(params.photoId)
        })}
        />
    </Route>
  )
}

const pageRoute = (page) => {
  return (
    <Route path={page.alias}
      components={{
        header: PageHeader,
        body: Page
      }}
      page={page}
    />
  )
}


const mainRoute = {
  path: '/',
  component: Main,
  indexRoute: { components: {header: HomeHeader, body: Home} },
  resolve() {
    return utils.fetchAll({
      categories: categoryService.fetchCategories(),
      pages: pageService.fetchPages()
    })
  },

  getChildRoutes(location, cb) {
    this.resolve().then(data => {
      cb(null, [].concat(
          data.categories.map(c => categryRoute(c)),
          data.pages.map(p => pageRoute(p))
        ));
    })
  }
}

export default mainRoute;
// (
//     <Route path="/" component={Main} resolve={params => ({
//         categories: categoryService.fetchCategories(),
//         pages: pageService.fetchPages()
//       })}
//       > //(:locale)
//       <IndexRoute components={{header: HomeHeader, body: Home}} />

//     </Route>
//   //   <Route path="*" component={NoMatch} />
// );
