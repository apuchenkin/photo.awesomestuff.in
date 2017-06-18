import 'rxjs/add/operator/mergeMap';
import { LOAD_PHOTOS, photosLoaded } from './actions';

export default (action$, store) =>
  action$
    .ofType(LOAD_PHOTOS)
    .mergeMap(({ category }) => store.getState().runtime
      .categoryService.fetchPhotos(category)
      .then(photosLoaded),
    );
