import Promise from 'promise';
import IntlMessageFormat from 'intl-messageformat';
import config from '../config.json';
import Picker from '../components/common/langs';

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

  getMeta(routes = [], messages, pathname) {
    const
      route = routes[routes.length - 1],
      meta = route.getMeta && route.getMeta() || {},
      description = new IntlMessageFormat(messages['meta.description']),
      hostname = config.hostname,
      langs = route.getLangs && route.getLangs() || config.locales;

    return {
      title: meta.title ? `${meta.title} - ${config.title}` : config.title,
      description: meta.description || description.format(),
      links: [
        `<link href="${pathname.replace(Picker.localeURL, `${hostname}$3`)}" rel="alternate" hreflang="x-default" />`
      ].concat(langs.map(lang => `<link href="${pathname.replace(Picker.localeURL, `${hostname}/${lang}$3`)}" rel="alternate" hreflang="${lang}" />`))
    };
  }
};
