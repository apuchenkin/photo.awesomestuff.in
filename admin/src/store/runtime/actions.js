export const SET_RUNTIME_VARIABLE = 'SET_RUNTIME_VARIABLE';
export const INIT = 'INIT';

export function init(token) {
  return {
    type: INIT,
    token,
  };
}

export function setRuntimeVariable({ name, value }) {
  return {
    type: SET_RUNTIME_VARIABLE,
    name,
    value,
  };
}
