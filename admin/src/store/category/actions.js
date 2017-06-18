export const LOAD_CATEGORIES = 'LOAD_CATEGORIES';
export const CATEGORIES_LOADED = 'CATEGORIES_LOADED';
export const ADD_CATEGORY = 'ADD_CATEGORY';

export const loadCategories = () => ({
  type: LOAD_CATEGORIES,
});

export const categoriesLoaded = categories => ({
  type: CATEGORIES_LOADED,
  categories,
});

export const categoryAdd = category => ({
  type: ADD_CATEGORY,
  category,
});
