export const LOAD = 'LOAD/TRANSLATION';
export const LOADED = 'LOADED/TRANSLATION';
export const CREATE = 'CREATE/TRANSLATION';
export const CREATED = 'CREATED/TRANSLATION';
export const UPDATE = 'UPDATE/TRANSLATION';
export const UPDATED = 'UPDATED/TRANSLATION';
export const REMOVE = 'REMOVE/TRANSLATION';
export const REMOVED = 'REMOVED/TRANSLATION';

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
