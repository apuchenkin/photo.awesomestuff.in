import R from 'ramda';

export const loaded = namespace => lens => (state, action) => ({
  ...state,
  [namespace]: lens(action),
});

export const created = namespace => lens => (state, action) => ({
  ...state,
  [namespace]: [...state[namespace], lens(action)],
});

export const updated = namespace => lens => equals => (state, action) => {
  const entity = lens(action);
  const list = state[namespace];

  return ({
    ...state,
    [namespace]: R.update(
      list.findIndex(equals(entity)),
      entity,
    )(list),
  });
};

export const removed = namespace => lens => equals => (state, action) => ({
  ...state,
  [namespace]: state[namespace].filter(R.complement(equals(lens(action)))),
});
