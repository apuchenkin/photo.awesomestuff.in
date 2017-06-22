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
    .mergeMap(() => store.getState().runtime
      .categoryService.fetchCategories()
      .then(loaded),
    ).catch(err => Observable.of(error(err)))
;

const create = (action, store) =>
  action
    .ofType(CREATE)
    .mergeMap(({ data }) => store.getState().runtime
      .categoryService.create(data)
      .then(created),
    ).catch(err => Observable.of(error(err)))
;

const update = (action, store) =>
  action
    .ofType(UPDATE)
    .mergeMap(({ category, data }) => store.getState().runtime
      .categoryService.update(category, data)
      .then(updated),
    ).catch(err => Observable.of(error(err)))
;

const remove = (action, store) =>
  action
    .ofType(REMOVE)
    .mergeMap(({ category }) => store.getState().runtime
      .categoryService.delete(category)
      .then(() => removed(category)),
    ).catch(err => Observable.of(error(err)))
;

export default combineEpics(
  load,
  create,
  update,
  remove,
);