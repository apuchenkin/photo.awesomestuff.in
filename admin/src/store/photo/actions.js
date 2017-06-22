export const LOAD = 'PHOTO/LOAD';
export const LOADED = 'PHOTO/LOADED';
export const UPDATE = 'PHOTO/UPDATE';
export const UPDATED = 'PHOTO/UPDATED';
export const ERROR = 'PHOTO/ERROR';

export const load = category => ({
  type: LOAD,
  category,
});

export const loaded = photos => ({
  type: LOADED,
  photos,
});

export const update = (photo, data) => ({
  type: UPDATE,
  photo,
  data,
});

export const updated = photo => ({
  type: UPDATED,
  photo,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
