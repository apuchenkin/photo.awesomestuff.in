export const LOAD = 'LOAD/PHOTO';
export const LOADED = 'LOADED/PHOTO';
export const UPDATE = 'UPDATE/PHOTO';
export const UPDATED = 'UPDATED/PHOTO';

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
