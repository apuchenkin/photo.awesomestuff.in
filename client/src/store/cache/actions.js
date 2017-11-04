export const CACHE = 'PAGE/CACHE';

export const cache = (data, indexes) => ({
  type: CACHE,
  data,
  indexes,
});
