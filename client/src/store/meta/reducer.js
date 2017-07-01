import { SET_META } from './actions';

const setMeta = (state, { meta }) => ({
  ...state,
  ...meta,
});

export default (state = {}, action) => {
  const reducer = {
    [SET_META]: setMeta,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
