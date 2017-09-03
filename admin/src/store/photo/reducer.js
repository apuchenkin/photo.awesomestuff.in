import R from 'ramda';
import PhotoService from 'common/service/api/Photo';

import { loaded, updated } from '../util/listReducer';
import { LOADED, UPDATED, ERROR } from './actions';

const initial = {
  total: 0,
  photos: [],
  groups: [],
};

const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};
const equals = p1 => p2 => p1.id === p2.id;

const loaded$ = (state, action) => {
  const response = action.photos.__response;
  const total = response.headers.get('x-total-count') || action.photos.length;
  const state$ = loaded('photos')(R.prop('photos'))(
    { ...state, total }, action);
  return loaded('groups')(({ photos }) => PhotoService.groupColors(photos))(state$, action);
};

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded$,
    [UPDATED]: updated('photos')(R.prop('photo'))(equals),
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
