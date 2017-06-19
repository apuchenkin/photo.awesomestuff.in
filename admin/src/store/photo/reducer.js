import Immutable from 'seamless-immutable';
import PhotoService from '../../../../client/lib/service/Photo';
import { LOADED, UPDATED } from './actions';

const initial = Immutable({
  photos: [],
  groups: [],
});

const loaded = (state, { photos }) => state
  .set('photos', Immutable(photos))
  .set('groups', PhotoService.groupColors(photos))
;

const updated = (state, { photo }) => state.set('photos',
  state.photos.set(state.photos.findIndex(p => p.id === photo.id), photo),
);

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded,
    [UPDATED]: updated,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
