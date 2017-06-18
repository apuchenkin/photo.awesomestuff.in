import Immutable from 'seamless-immutable';

import { ADD, LOADED, UPDATED, DELETED } from './actions';

const initial = Immutable({
  categories: [],
});

const categoriesLoaded = (state, { categories }) => state.set('categories', Immutable(categories));
const categoryAdd = (state, { category }) => state.set('categories', Immutable([...state.categories, category]));
const categoryUpdated = (state, { category }) => state.set('categories',
  state.categories.set(state.categories.findIndex(c => c.id === category.id), category),
);
const categoryDeleted = (state, { category }) =>
  state.set('categories', Immutable(state.categories.filter(c => c.id !== category.id)));

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: categoriesLoaded,
    [ADD]: categoryAdd,
    [UPDATED]: categoryUpdated,
    [DELETED]: categoryDeleted,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
