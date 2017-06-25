export const LOAD_ALL = 'PHOTO/LOAD/ALL';
export const LOADED_ALL = 'PHOTO/LOADED/ALL';

export const LOAD = 'PHOTO/LOAD';
export const LOADED = 'PHOTO/LOADED';

export const CANCELLED = 'PHOTO/CANCELLED';
export const ERROR = 'PHOTO/ERROR';

export const loadAll = (category, resolve, reject) => ({
  type: LOAD_ALL,
  meta: { resolve, reject },
  category,
});

export const loadedAll = photos => ({
  type: LOADED_ALL,
  photos,
});

export const load = (id, resolve, reject) => ({
  type: LOAD,
  meta: { resolve, reject },
  id,
});

export const loaded = photo => ({
  type: LOADED,
  photo,
});

export const cancelled = () => ({
  type: CANCELLED,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
