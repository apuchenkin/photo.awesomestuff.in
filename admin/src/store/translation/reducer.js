import R from 'ramda';
import { LOADED, CREATED, UPDATED, REMOVED, ERROR } from './actions';
import { loaded, updated, created, removed } from '../util/listReducer';

const initial = {
  translations: [],
};

const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};

const equals = t1 => t2 => t1.id === t2.id;

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded('translations')(R.prop('translations')),
    [CREATED]: created('translations')(R.prop('translation')),
    [UPDATED]: updated('translations')(R.prop('translation'))(equals),
    [REMOVED]: removed('translations')(R.prop('translation'))(equals),
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
