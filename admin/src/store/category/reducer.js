import Immutable from 'seamless-immutable';

import { LOADED, CREATED, UPDATED, REMOVED, ERROR } from './actions';

const initial = Immutable({
  categories: [],
});

const loaded = (state, { categories }) => state.set('categories', Immutable(categories));
const created = (state, { category }) => state.set('categories', Immutable([...state.categories, category]));
const updated = (state, { category }) => state.set('categories',
  state.categories.set(state.categories.findIndex(c => c.id === category.id), category),
);
const removed = (state, { category }) =>
  state.set('categories', Immutable(state.categories.filter(c => c.id !== category.id)));

const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded,
    [CREATED]: created,
    [UPDATED]: updated,
    [REMOVED]: removed,
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
