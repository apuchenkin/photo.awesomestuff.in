import { LOAD, LOADED, CANCELLED, ERROR } from './actions';

const initial = {
  categories: [],
  loading: false,
};

const load = state => ({ ...state, loading: true });
const loaded = (state, { categories }) => ({ ...state, categories, loading: false });
const cancelled = state => ({ ...state, loading: false });
const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};

export default (state = initial, action) => {
  const reducer = {
    [LOAD]: load,
    [LOADED]: loaded,
    [CANCELLED]: cancelled,
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
