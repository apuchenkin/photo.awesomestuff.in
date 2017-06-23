import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
import 'rxjs/add/observable/of';
import { combineEpics } from 'redux-observable';

import {
  LOAD, CANCELLED,
  loaded, error,
} from './actions';

const load = (action, store, { categoryService }) =>
  action
    .ofType(LOAD)
    .mergeMap(() => categoryService
      .fetchCategories()
      .then(loaded),
    )
    .takeUntil(action.ofType(CANCELLED))
    .catch(err => Observable.of(error(err)))
;

export default combineEpics(
  load,
);
