export const LOAD = 'LOAD';
export const LOADED = 'LOADED';
export const CREATE = 'CREATE';
export const ADD = 'ADD';
export const UPDATE = 'UPDATE';
export const UPDATED = 'UPDATED';
export const DELETE = 'DELETE';
export const DELETED = 'DELETED';

export const loadCategories = () => ({
  type: LOAD,
});

export const categoriesLoaded = categories => ({
  type: LOADED,
  categories,
});

export const categoryCreate = data => ({
  type: CREATE,
  data,
});

export const categoryUpdate = (category, data) => ({
  type: UPDATE,
  category,
  data,
});

export const categoryUpdated = category => ({
  type: UPDATED,
  category,
});

export const categoryAdd = category => ({
  type: ADD,
  category,
});

export const categoryDelete = category => ({
  type: DELETE,
  category,
});

export const categoryDeleted = category => ({
  type: DELETED,
  category,
});
