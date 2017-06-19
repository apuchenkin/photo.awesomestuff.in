export const LOAD = 'LOAD/CATEGORY';
export const LOADED = 'LOADED/CATEGORY';
export const CREATE = 'CREATE/CATEGORY';
export const CREATED = 'CREATED/CATEGORY';
export const UPDATE = 'UPDATE/CATEGORY';
export const UPDATED = 'UPDATED/CATEGORY';
export const REMOVE = 'REMOVE/CATEGORY';
export const REMOVED = 'REMOVED/CATEGORY';

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
