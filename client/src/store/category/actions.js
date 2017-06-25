export const LOAD_ALL = 'CATEGORY/LOAD/ALL';
export const LOADED_ALL = 'CATEGORY/LOADED/ALL';

export const LOAD = 'CATEGORY/LOAD';
export const LOADED = 'CATEGORY/LOADED';

export const CANCELLED = 'CATEGORY/CANCELLED';
export const ERROR = 'CATEGORY/ERROR';

export const loadAll = (resolve, reject) => ({
  type: LOAD_ALL,
  meta: { resolve, reject },
});

export const loadedAll = categories => ({
  type: LOADED_ALL,
  categories,
});

export const load = (name, resolve, reject) => ({
  type: LOAD,
  meta: { resolve, reject },
  name,
});

export const loaded = category => ({
  type: LOADED,
  category,
});

export const cancelled = () => ({
  type: CANCELLED,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
