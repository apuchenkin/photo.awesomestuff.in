import { LOAD_ALL, LOADED_ALL, LOAD, LOADED, CANCELLED } from './actions';

const initial = {
  photos: [],
  category: null,
  photo: null,
  loading: false,
};

const load = state => ({ ...state, loading: true });
const loadAll = state => ({ ...state, loading: true });
const loaded = (state, { photo }) => ({ ...state, photo, loading: false });
const loadedAll = (state, { photos, category }) => ({ ...state, photos, category, loading: false });
const cancelled = state => ({ ...state, loading: false });
// const error = (state, { error }) => {
//   console.log(action.error); // eslint-disable-line no-console
//   return state;
// };

export default (state = initial, action) => {
  const reducer = {
    [LOAD]: load,
    [LOADED]: loaded,
    [LOAD_ALL]: loadAll,
    [LOADED_ALL]: loadedAll,
    [CANCELLED]: cancelled,
    // [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
