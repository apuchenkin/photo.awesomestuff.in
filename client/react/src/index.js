import React from 'react';
import ReactDOM from 'react-dom';
import Router from 'react-router/lib/Router';
import match from 'react-router/lib/match';
import RouterContext from 'react-router/lib/RouterContext';
import useRouterHistory from 'react-router/lib/useRouterHistory';
import createHistory from 'history/lib/createBrowserHistory';
import { IntlProvider, addLocaleData } from 'react-intl';
import ruLocaleData from 'react-intl/locale-data/ru';

import createRoutes from './routes';
import config from './config/config';
import utils from './lib/utils';
import LoadingContext from './components/loadingContext';

import './assets/fontello/css/fontello.css';
import './style/main.less';

addLocaleData(ruLocaleData);

const
  isBrowser = (typeof window !== 'undefined'),
  initialState = isBrowser && (window.__INITIAL_STATE__ || {}),
  locale = initialState.locale || config.fallbackLocale,
  basename = initialState.basename,
  messages = initialState.messages,
  history = useRouterHistory(createHistory)({
    basename,
  });

function createElement(component, props) {
  return component(props);
}

function metaUpdate(meta) {
  document.title = meta.title;
  document.head.querySelector('meta[name=description]').content = meta.description;
  Array.from(document.head.querySelectorAll('link[hreflang]')).map(node => document.head.removeChild(node));
  meta.links.map(link => document.head.insertAdjacentHTML('beforeend', link));
}

function onUpdate() {
  const
    { routes, location } = this.state,
    meta = utils.getMeta(routes, messages, location.pathname);

  metaUpdate(meta);
  ga('send', 'pageview', {
    title: meta.title,
    page: location.pathname,
  });

  this.props.stopLoading();
}

function doRender(props) {
  return <RouterContext {...props} />;
}


match({ history, routes: createRoutes(locale) }, (error, redirectLocation, renderProps) => {
  ReactDOM.render(
    <IntlProvider locale={locale} messages={messages}>
      <LoadingContext history={history}>
        <Router
          {...renderProps}
          createElement={createElement}
          render={doRender}
          onUpdate={onUpdate}
        />
      </LoadingContext>
    </IntlProvider>,
    document.getElementById('react-view')
  );
});
