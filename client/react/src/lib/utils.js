import Promise from 'promise';
import IntlMessageFormat from 'intl-messageformat';
import config from '../config.json';

export default {
  pick(object, params) {
    return Object.keys(object).filter(k => params.indexOf(k) >= 0).reduce((o,k) => {o[k] = object[k]; return o;}, {});
  },

  omit(object, params = []) {
    return this.pick(object, Object.keys(object).filter(k => params.indexOf(k) === -1));
  },

  fetchAll(object) {
    const keys = Object.keys(object);

    return Promise.all(keys.map(k => object[k])).then(data => {
      return keys.reduce((acc, k, idx) => {acc[k] = data[idx]; return acc;}, {});
    });
  },

  getMeta(routes = [], messages) {
    const
      route = routes[routes.length - 1],
      meta = route.getMeta && route.getMeta() || {},
      description = new IntlMessageFormat(messages['meta.description']);

    return {
      title: meta.title ? `${meta.title} - ${config.title}` : config.title,
      description: meta.description || description.format()
    };
  }
};
