import { combineEpics } from 'redux-observable';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
import 'rxjs/add/operator/do';
import 'rxjs/add/operator/map';
import 'rxjs/add/observable/of';
import 'rxjs/add/observable/from';

import {
  LOAD_ALL, LOAD, CANCELLED,
  loadedAll, loaded, error,
} from './actions';
import { setRuntimeVariable } from '../runtime/actions';

const loadAll = (action, store, { categoryService }) =>
  action
    .ofType(LOAD_ALL)
    .mergeMap(({ meta }) =>
      Observable.from(categoryService.fetchCategories())
        .do({
          next: meta.resolve,
          error: meta.reject,
        })
        .map(loadedAll)
        .takeUntil(action.ofType(CANCELLED))
        .catch(err => Observable.of(error(err))),
    )
;

const load = (action$, store, { categoryService }) =>
  action$
    .ofType(LOAD)
    .mergeMap(({ meta, name }) =>
      Observable.from(categoryService.fetchCategory(name))
        .do({
          next: meta.resolve,
          error: meta.reject,
        })
        .concatMap(category => [
          loaded(category),
          setRuntimeVariable('langs', category.langs),
        ])
        .takeUntil(action$.ofType(CANCELLED))
        .catch(err => Observable.of(error(err))),
    )
;

export default combineEpics(
  loadAll,
  load,
);
