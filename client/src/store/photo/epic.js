import { combineEpics } from 'redux-observable';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
import 'rxjs/add/operator/do';
import 'rxjs/add/operator/map';
import 'rxjs/add/operator/concatMap';
import 'rxjs/add/observable/of';
import 'rxjs/add/observable/from';

import { remapPhotos, refinePhotos } from '../../../lib/service/Photo';

import {
  LOAD_ALL, LOAD, CANCELLED,
  loadedAll, loaded, error,
} from './actions';
import { setRuntimeVariable } from '../runtime/actions';

const loadAll = (action, store, { categoryService }) => {
  const config = store.getState().runtime.config;

  return action
    .ofType(LOAD_ALL)
    .mergeMap(({ meta, category }) =>
      Observable.from(
        categoryService.fetchPhotos(category)
          .then(refinePhotos) // TODO: photoId
          .then(remapPhotos({ width: config.brickWidth, gutter: config.gutter })),
      )
        .do({
          next: meta.resolve,
          error: meta.reject,
        })
        .map(loadedAll)
        .takeUntil(action.ofType(CANCELLED))
        .catch(err => Observable.of(error(err))),
    );
};

const load = (action$, store, { photoService, intl }) =>
  action$
    .ofType(LOAD)
    .mergeMap(({ meta, id }) =>
      Observable.from(photoService.fetchPhoto(id))
        .do({
          next: meta.resolve,
          error: meta.reject,
        })
        .concatMap(photo => [
          loaded(photo),
          setRuntimeVariable('langs', photo.langs),
          setRuntimeVariable('meta', {
            title: photo.description,
            description: intl.formatMessage({ id: 'meta.description.photo' }, {
              author: photo.author && photo.author.name,
              title: photo.description,
            }),
          }),
        ])
        .takeUntil(action$.ofType(CANCELLED))
        .catch(err => Observable.of(error(err))),
    )
;

export default combineEpics(
  loadAll,
  load,
);
