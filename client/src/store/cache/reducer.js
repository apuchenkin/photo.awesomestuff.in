import { CACHE } from './actions';

const initial = {
  cache: {},
};

const foldIndexes = (cache = {}, data, indexes) => {
  const [i, ...is] = indexes;
  const [x, ...xs] = data;

  return {
    ...cache,
    [i]: {
      data: x,
      ...(is.length && foldIndexes(null, xs, is)),
    },
  };
};

const setCache = ({ cache, ...state }, { data = [], indexes = [] }) => ({
  ...state,
  cache: foldIndexes(cache, data, indexes),
});

export default (state = initial, action) => {
  const reducer = {
    [CACHE]: setCache,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
