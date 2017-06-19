import 'rxjs/add/operator/mergeMap';
import { combineEpics } from 'redux-observable';
import {
  LOAD, CREATE, UPDATE, REMOVE,
  loaded, created, updated, removed,
} from './actions';

const load = (action, store) =>
  action
    .ofType(LOAD)
    .mergeMap(({ query }) => store.getState().runtime
      .translationService.load(query)
      .then(loaded),
    )
;

const create = (action, store) =>
  action
    .ofType(CREATE)
    .mergeMap(({ data }) => store.getState().runtime
      .translationService.create(data)
      .then(created),
    )
;

const update = (action, store) =>
  action
    .ofType(UPDATE)
    .mergeMap(({ translation, data }) => store.getState().runtime
      .translationService.update(translation, data)
      .then(updated),
    )
;

const remove = (action, store) =>
  action
    .ofType(REMOVE)
    .mergeMap(({ translation }) => store.getState().runtime
      .translationService.delete(translation)
      .then(() => removed(translation)),
    )
;

export default combineEpics(
  load,
  create,
  update,
  remove,
);
