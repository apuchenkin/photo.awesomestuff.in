export const LOAD = 'CATEGORY/LOAD';
export const LOADED = 'CATEGORY/LOADED';
export const CANCELLED = 'CATEGORY/CANCELLED';
export const ERROR = 'CATEGORY/ERROR';

export const load = () => ({
  type: LOAD,
});

export const loaded = categories => ({
  type: LOADED,
  categories,
});

export const cancelled = () => ({
  type: CANCELLED,
});

export const error = err => ({
  type: ERROR,
  error: err,
});
