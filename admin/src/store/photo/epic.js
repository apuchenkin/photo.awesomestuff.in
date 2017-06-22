import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/observable/of';
import { combineEpics } from 'redux-observable';
import {
  LOAD, UPDATE,
  loaded, updated, error,
} from './actions';

const load = (action, store) =>
  action
    .ofType(LOAD)
    .mergeMap(({ category }) => store.getState().runtime
      .categoryService.fetchPhotos(category)
      .then(loaded),
    ).catch(err => Observable.of(error(err)))
;

const update = (action, store) =>
  action
    .ofType(UPDATE)
    .mergeMap(({ photo, data }) => store.getState().runtime
      .photoService.update(photo, data)
      .then(updated),
    ).catch(err => Observable.of(error(err)))
;

export default combineEpics(
  load,
  update,
);
