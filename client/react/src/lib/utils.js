import Promise from 'promise';

export default {
  pick(object, params) {
  	return Object.keys(object).filter(k => params.indexOf(k) >= 0).reduce((o,k) => {o[k] = object[k]; return o}, {})
  },

  fetchAll(object, cache) {
    const
      keys = Object.keys(object),
      val = this.pick(cache, keys)
    ;

    return Object.keys(val).length
      ? Promise.resolve(val)
      : Promise.all(keys.map(k => object[k]())).then(data => {
          return keys.reduce((acc, k, idx) => {acc[k] = data[idx]; return acc}, {});
        });
  }
}
