import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/mergeMap';
import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/takeUntil';
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
    .mergeMap(({ page }) => {
      const page$ = store.getState().page.page;
      if (page$ && page$.alias === page.alias) {
        return Observable.of(loaded(page$));
      }

      return Observable.from(pageService.fetchPage(page))
        .map(loaded)
        .takeUntil(action$.ofType(CANCELLED))
        .catch(err => Observable.of(error(err)))
      ;
    })
;

export default load;
