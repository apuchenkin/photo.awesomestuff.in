export const LOAD = 'CATEGORY/LOAD';
export const LOADED = 'CATEGORY/LOADED';
export const CREATE = 'CATEGORY/CREATE';
export const CREATED = 'CATEGORY/CREATED';
export const UPDATE = 'CATEGORY/UPDATE';
export const UPDATED = 'CATEGORY/UPDATED';
export const REMOVE = 'CATEGORY/REMOVE';
export const REMOVED = 'CATEGORY/REMOVED';
export const ERROR = 'CATEGORY/ERROR';

export const load = () => ({
  type: LOAD,
});

export const loaded = categories => ({
  type: LOADED,
  categories,
});

export const create = data => ({
  type: CREATE,
  data,
});

export const created = category => ({
  type: CREATED,
  category,
});

export const update = (category, data) => ({
  type: UPDATE,
  category,
  data,
});

export const updated = category => ({
  type: UPDATED,
  category,
});

export const remove = category => ({
  type: REMOVE,
  category,
});

export const removed = category => ({
  type: REMOVED,
  category,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
