import React from 'react';
import { Promise } from 'es6-promise';
import IntlMessageFormat from 'intl-messageformat';

import config from '../config/config.json';
import Picker from '../components/common/langs';

export default {
  pick(object, params) {
    return Object.keys(object)
    .filter(k => params.indexOf(k) >= 0)
    .reduce((o, k) => Object.assign(o, { [k]: object[k] }), {});
  },

  omit(object, params = []) {
    return this.pick(object, Object.keys(object).filter(k => params.indexOf(k) === -1));
  },

  fetchAll(object) {
    const keys = Object.keys(object);

    return Promise
      .all(keys.map(k => object[k]))
      .then(data => keys.reduce((acc, k, idx) => Object.assign(acc, { [k]: data[idx] }), {}));
  },

  getMeta(routes = [], messages, pathname) {
    const
      route = routes[routes.length - 1],
      meta = (route.getMeta && route.getMeta()) || {},
      description = new IntlMessageFormat(messages['meta.description']),
      hostname = config.hostname,
      langs = (route.getLangs && route.getLangs()) || config.locales;

    return {
      title: meta.title ? `${meta.title} - ${config.title}` : config.title,
      description: meta.description || description.format(),
      links: [
        <link href={pathname.replace(Picker.localeURL, `${hostname}$3`)} rel="alternate" hrefLang="x-default" key="x-default" />,
      ].concat(langs.map(lang =>
        <link href={pathname.replace(Picker.localeURL, `${hostname}/${lang}$3`)} rel="alternate" hrefLang={lang} key={lang} />
      )),
    };
  },
};
