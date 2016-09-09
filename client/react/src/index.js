import React from 'react';
import ReactDOM from 'react-dom';
import Router from 'react-router/lib/Router';
import match from 'react-router/lib/match';
import useRouterHistory from 'react-router/lib/useRouterHistory';
import { createHistory } from 'history';
import { IntlProvider, addLocaleData }  from 'react-intl';
import ruLocaleData from 'react-intl/locale-data/ru';

import createRoutes from './routes';
import config from './config';
import utils from './lib/utils';

require('./polyfill/find');

import './assets/fontello/css/fontello.css';
import './style/main.less';

addLocaleData(ruLocaleData);

const createElement = (component, props) => component(props);

const
  isBrowser = (typeof window !== 'undefined'),
  initialState = isBrowser && window.__INITIAL_STATE__ || {},
  locale = initialState.locale || config.fallbackLocale,
  routes = createRoutes(locale),
  basename = initialState.basename,
  messages = initialState.messages,
  history = useRouterHistory(createHistory)({
    basename
  });

function onUpdate() {
  let
    {routes, location} = this.state,
    meta = utils.getMeta(routes, messages, location.pathname);

  metaUpdate(meta);
}

function metaUpdate(meta) {
  document.title = meta.title;
  document.head.querySelector('meta[name=description]').content = meta.description;
  Array.from(document.head.querySelectorAll('link[hreflang]')).map(node => document.head.removeChild(node));
  meta.links.map(link => document.head.insertAdjacentHTML('beforeend', link));
}

match({ history, routes}, (error, redirectLocation, renderProps) => {
  ReactDOM.render(
    <IntlProvider locale={locale} messages={messages}>
      <Router {...renderProps} createElement={createElement} onUpdate={onUpdate} />
    </IntlProvider>,
    document.getElementById('react-view')
  );
});
