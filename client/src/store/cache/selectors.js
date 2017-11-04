
export const getCache = (cache, indexes = []) => indexes
  .reduce((c, i) => c[i] || {}, cache).data;

export default {};
