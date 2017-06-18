import 'rxjs/add/operator/mergeMap';
import { combineEpics } from 'redux-observable';
import {
  LOAD, CREATE, UPDATE, DELETE,
  categoriesLoaded, categoryAdd, categoryUpdated, categoryDeleted,
} from './actions';

const load = (action$, store) =>
  action$
    .ofType(LOAD)
    .mergeMap(() => store.getState().runtime
      .categoryService.fetchCategories()
      .then(categoriesLoaded),
    )
;

const create = (action$, store) =>
  action$
    .ofType(CREATE)
    .mergeMap(({ data }) => store.getState().runtime
      .categoryService.create(data)
      .then(categoryAdd),
    )
;

const update = (action$, store) =>
  action$
    .ofType(UPDATE)
    .mergeMap(({ category, data }) => store.getState().runtime
      .categoryService.update(category, data)
      .then(categoryUpdated),
    )
;

const del = (action$, store) =>
  action$
    .ofType(DELETE)
    .mergeMap(({ category }) => store.getState().runtime
      .categoryService.delete(category)
      .then(() => categoryDeleted(category)),
    )
;

export default combineEpics(
  load,
  create,
  update,
  del,
);
