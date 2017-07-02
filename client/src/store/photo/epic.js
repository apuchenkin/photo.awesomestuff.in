import { combineEpics } from 'redux-observable';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
import 'rxjs/add/operator/do';
import 'rxjs/add/operator/map';
import 'rxjs/add/observable/of';
import 'rxjs/add/observable/from';
import { remapPhotos, refinePhotos } from '../../../lib/service/Photo';

import {
  LOAD_ALL, LOAD, CANCELLED,
  loadedAll, loaded, error,
} from './actions';

const loadAll = (action, store, { categoryService }) => {
  const config = store.getState().runtime.config;

  return action
    .ofType(LOAD_ALL)
    .mergeMap(({ category, photoId }) => {
      const { photo } = store.getState();
      if (photo.category && photo.category.id === category.id && photo.photos.length) {
        return Observable.of(loadedAll(category)(photo.photos));
      }

      return Observable.from(
        categoryService.fetchPhotos(category)
          .then(refinePhotos(photoId))
          .then(remapPhotos({ width: config.brickWidth, gutter: config.gutter })),
      )
      .map(loadedAll(category))
      .takeUntil(action.ofType(CANCELLED))
      .catch(err => Observable.of(error(err)));
    });
};

const load = (action$, store, { photoService }) =>
  action$
    .ofType(LOAD)
    .mergeMap(({ id }) => {
      const { photo: { photo } } = store.getState();
      if (photo && photo.id === Number(id)) {
        return Observable.of(loaded(photo));
      }

      return Observable.from(photoService.fetchPhoto(id).catch())
        .map(loaded)
        .takeUntil(action$.ofType(CANCELLED))
        .catch(err => Observable.of(error(err)));
    })
;

export default combineEpics(
  loadAll,
  load,
);
