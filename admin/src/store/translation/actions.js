export const LOAD = 'TRANSLATION/LOAD';
export const LOADED = 'TRANSLATION/LOADED';
export const CREATE = 'TRANSLATION/CREATE';
export const CREATED = 'TRANSLATION/CREATED';
export const UPDATE = 'TRANSLATION/UPDATE';
export const UPDATED = 'TRANSLATION/UPDATED';
export const REMOVE = 'TRANSLATION/REMOVE';
export const REMOVED = 'TRANSLATION/REMOVED';
export const ERROR = 'TRANSLATION/ERROR';

export const load = query => ({
  type: LOAD,
  query,
});

export const loaded = translations => ({
  type: LOADED,
  translations,
});

export const create = data => ({
  type: CREATE,
  data,
});

export const created = translation => ({
  type: CREATED,
  translation,
});

export const update = (translation, data) => ({
  type: UPDATE,
  translation,
  data,
});

export const updated = translation => ({
  type: UPDATED,
  translation,
});

export const remove = translation => ({
  type: REMOVE,
  translation,
});

export const removed = translation => ({
  type: REMOVED,
  translation,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
