import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
import 'rxjs/add/operator/do';
import 'rxjs/add/operator/map';
import 'rxjs/add/observable/of';
import 'rxjs/add/observable/from';

import {
  LOAD, CANCELLED,
  loaded, error,
} from './actions';

const load = (action$, store, { pageService }) =>
  action$
    .ofType(LOAD)
    .mergeMap(({ meta, page }) =>
      Observable.from(pageService.fetchPage(page))
        .do({
          next: meta.resolve,
          error: meta.reject,
        })
        .map(loaded)
        .takeUntil(action$.ofType(CANCELLED))
        .catch(err => Observable.of(error(err))),
    )
;

export default load;
