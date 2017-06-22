import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/observable/of';
import { combineEpics } from 'redux-observable';
import {
  LOAD, CREATE, UPDATE, REMOVE,
  loaded, created, updated, removed, error,
} from './actions';

const load = (action, store) =>
  action
    .ofType(LOAD)
    .mergeMap(({ query }) => store.getState().runtime
      .translationService.load(query)
      .then(loaded),
    ).catch(err => Observable.of(error(err)))
;

const create = (action, store) =>
  action
    .ofType(CREATE)
    .mergeMap(({ data }) => store.getState().runtime
      .translationService.create(data)
      .then(created),
    ).catch(err => Observable.of(error(err)))
;

const update = (action, store) =>
  action
    .ofType(UPDATE)
    .mergeMap(({ translation, data }) => store.getState().runtime
      .translationService.update(translation, data)
      .then(updated),
    ).catch(err => Observable.of(error(err)))
;

const remove = (action, store) =>
  action
    .ofType(REMOVE)
    .mergeMap(({ translation }) => store.getState().runtime
      .translationService.delete(translation)
      .then(() => removed(translation)),
    ).catch(err => Observable.of(error(err)))
;

export default combineEpics(
  load,
  create,
  update,
  remove,
);
