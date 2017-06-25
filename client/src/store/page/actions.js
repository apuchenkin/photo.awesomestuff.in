export const LOAD = 'PAGE/LOAD';
export const LOADED = 'PAGE/LOADED';
export const CANCELLED = 'PAGE/CANCELLED';
export const ERROR = 'PAGE/ERROR';

export const load = (page, resolve, reject) => ({
  type: LOAD,
  meta: { resolve, reject },
  page,
});

export const loaded = page => ({
  type: LOADED,
  page,
});

export const cancelled = () => ({
  type: CANCELLED,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
