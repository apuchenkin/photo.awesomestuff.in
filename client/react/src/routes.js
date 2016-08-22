import React from 'react';
import { Route, Redirect, createRoutes, withRouter, IndexRoute } from 'react-router';
import CategoryService from './service/Category';
import PhotoService from './service/Photo';
import Home from './components/home';
import HomeHeader from './components/home/header';
import Gallery from './components/gallery';
import GalleryHeader from './components/gallery/header';
import Photo from './components/photo';
import Link from 'react-router/lib/Link';
import Main from './components/main';

import './style/main.less';

const categoryService = new CategoryService();
const photoService = new PhotoService();


class NoMatch extends React.Component {
  render() {
    return (
      <div>NoMatch</div>
    )
  }
}

export default (
    <Route path="/" component={Main} resolve={params => ({
        categories: categoryService.fetchCategories()
      })}
      > //(:locale)
      <IndexRoute components={{header: HomeHeader, body: Home}} />
      <Route path=":category(/:subcategory)"
        components={{header: GalleryHeader, body: Gallery}}
        resolve={params => ({
      			photos: photoService.fetchPhotos(params.subcategory || params.category).then(p =>
      				photoService.remapPhotos(
      					photoService.refinePhotos(p, params.photoId)
      				)
      			)
    		})}
        >
        <Route path="/:category(/:subcategory)/photo/:photoId"
          component={Photo}
          resolve={params => ({
            photo: photoService.fetchPhoto(params.photoId)
          })}
          />
      </Route>
    </Route>
  //   <Route path="*" component={NoMatch} />
);
