import 'rxjs/add/operator/mergeMap';
import { LOAD_CATEGORIES, categoriesLoaded } from './actions';

export default (action$, store) =>
  action$
    .ofType(LOAD_CATEGORIES)
    .mergeMap(() => store.getState().runtime
      .categoryService.fetchCategories()
      .then(categoriesLoaded),
    )
;
