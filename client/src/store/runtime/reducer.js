import { SET_RUNTIME_VARIABLE } from './actions';

const setRuntimeVariable = (state, { payload: { name, value } }) => ({
  ...state,
  [name]: value,
});

export default (state = {}, action) => {
  const reducer = {
    [SET_RUNTIME_VARIABLE]: setRuntimeVariable,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
