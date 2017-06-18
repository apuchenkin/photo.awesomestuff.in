import Immutable from 'seamless-immutable';

import { LOAD_CATEGORIES, CATEGORIES_LOADED, ADD_CATEGORY } from './actions';

const initial = Immutable({
  categories: [],
});

const categoriesLoad = state => state;
const categoriesLoaded = (state, { categories }) => state.set('categories', Immutable(categories));
const categoryAdd = (state, { category }) => state.set('categories', state.get('categories').push(category));

export default (state = initial, action) => {
  const reducer = {
    [LOAD_CATEGORIES]: categoriesLoad,
    [CATEGORIES_LOADED]: categoriesLoaded,
    [ADD_CATEGORY]: categoryAdd,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
