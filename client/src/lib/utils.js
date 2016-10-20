import React from 'react';
import { Promise } from 'es6-promise';
import IntlMessageFormat from 'intl-messageformat';

import { getSrc } from '../../lib/util/photo/memoize';
import config from '../etc/config.json';

export const localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g;

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

  getSrc(...args) {
    return [config.staticEndpoint, getSrc(...args)].join('/');
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
        <link href={`${hostname}${pathname}`} rel="alternate" hrefLang="x-default" key="x-default" />,
      ].concat(langs.map(lang =>
        <link href={`${hostname}/${lang}${pathname}`} rel="alternate" hrefLang={lang} key={lang} />
      )),
    };
  },
};
