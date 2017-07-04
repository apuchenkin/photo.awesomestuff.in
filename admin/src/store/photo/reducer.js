import Immutable from 'seamless-immutable';
import PhotoService from '../../../../common/service/api/Photo';
import { LOADED, UPDATED, ERROR } from './actions';

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

const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded,
    [UPDATED]: updated,
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
