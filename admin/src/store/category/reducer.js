import R from 'ramda';
import { LOADED, CREATED, UPDATED, REMOVED, ERROR } from './actions';
import { loaded, updated, created, removed } from '../util/listReducer';

const NAMESPACE = 'categories';

const initial = {
  [NAMESPACE]: [],
};

const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};

const equals = c1 => c2 => c1.id === c2.id;

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded(NAMESPACE)(R.prop('categories')),
    [CREATED]: created(NAMESPACE)(R.prop('category')),
    [UPDATED]: updated(NAMESPACE)(R.prop('category'))(equals),
    [REMOVED]: removed(NAMESPACE)(R.prop('category'))(equals),
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
