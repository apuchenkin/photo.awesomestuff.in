export const LOAD_ALL = 'PHOTO/LOAD/ALL';
export const LOADED_ALL = 'PHOTO/LOADED/ALL';

export const LOAD = 'PHOTO/LOAD';
export const LOADED = 'PHOTO/LOADED';

export const CANCELLED = 'PHOTO/CANCELLED';
export const ERROR = 'PHOTO/ERROR';

export const loadAll = (category, photoId) => ({
  type: LOAD_ALL,
  lifecycle: { resolve: LOADED_ALL, reject: ERROR },
  category,
  photoId,
});

export const loadedAll = category => photos => ({
  type: LOADED_ALL,
  photos,
  category,
});

export const load = id => ({
  type: LOAD,
  lifecycle: { resolve: LOADED, reject: ERROR },
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
