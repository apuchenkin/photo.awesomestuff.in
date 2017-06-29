import { SET_RUNTIME_VARIABLE } from './actions';

const log = (val) => {
  console.log(val);
  return val;
};

const setRuntimeVariable = (state, { payload: { name, value } }) => ({
  ...state,
  [name]: log(value),
});

export default (state = {}, action) => {
  const reducer = {
    [SET_RUNTIME_VARIABLE]: setRuntimeVariable,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
