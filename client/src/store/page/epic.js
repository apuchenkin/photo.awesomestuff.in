import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
import 'rxjs/add/operator/do';
import 'rxjs/add/operator/concatMap';
import 'rxjs/add/observable/of';
import 'rxjs/add/observable/from';

import {
  LOAD, CANCELLED,
  loaded, error,
} from './actions';
import { setRuntimeVariable } from '../runtime/actions';

const load = (action$, store, { pageService }) =>
  action$
    .ofType(LOAD)
    .mergeMap(({ meta, page }) =>
      Observable.from(pageService.fetchPage(page))
        .do({
          next: meta.resolve,
          error: meta.reject,
        })
        .concatMap(page$ => [
          loaded(page$),
          setRuntimeVariable('langs', page$.langs),
        ])
        .takeUntil(action$.ofType(CANCELLED))
        .catch(err => Observable.of(error(err))),
    )
;

export default load;
