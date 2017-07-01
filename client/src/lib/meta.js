import React from 'react';
import ReactDOM from 'react-dom/server';

const updateLinks = (links) => {
  Array.from(document.head.querySelectorAll('link[hreflang]')).map((node) => {
    document.head.removeChild(node);
    return false;
  });

  const links$ = links.reduce((acc, link) =>
    acc.concat(ReactDOM.renderToStaticMarkup(link)),
    [],
  );

  document.head.insertAdjacentHTML('beforeend', links$.join('\n'));
};

export const metaUpdate = (meta, location) => {
  document.title = meta.title;
  document.head.querySelector('meta[name=description]').content = meta.description;
  updateLinks(meta.links);

  if (typeof ga !== 'undefined') {
    ga('send', 'pageview', {
      title: meta.title,
      page: location.pathname,
    });
  }
};

export const buildMeta = (location, store, intl) => {
  const url = location.pathname;
  const { runtime: { config: { hostname, locales, title } }, meta } = store.getState();
  const langs = meta.langs || locales;

  return {
    title: meta.title ? `${meta.title} - ${title}` : title,
    description: meta.description || intl.formatMessage({ id: 'meta.description' }),
    links: [
      <link href={`${hostname}${url}`} rel="alternate" hrefLang="x-default" key="x-default" />,
    ].concat(langs.map(lang =>
      <link href={`${hostname}/${lang}${url}`} rel="alternate" hrefLang={lang} key={lang} />,
    )),
  };
};

export default {
  buildMeta,
  metaUpdate,
};
