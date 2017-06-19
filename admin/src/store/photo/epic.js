import 'rxjs/add/operator/mergeMap';
import { combineEpics } from 'redux-observable';
import {
  LOAD, UPDATE,
  loaded, updated,
} from './actions';

const load = (action, store) =>
  action
    .ofType(LOAD)
    .mergeMap(({ category }) => store.getState().runtime
      .categoryService.fetchPhotos(category)
      .then(loaded),
    )
;

const update = (action, store) =>
  action
    .ofType(UPDATE)
    .mergeMap(({ photo, data }) => store.getState().runtime
      .photoService.update(photo, data)
      .then(updated),
    )
;

export default combineEpics(
  load,
  update,
);
