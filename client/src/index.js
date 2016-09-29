import React from 'react';
import { Provider } from 'react-redux';
import ReactDOM from 'react-dom';
import Router from 'react-router/lib/Router';
import match from 'react-router/lib/match';
import useRouterHistory from 'react-router/lib/useRouterHistory';
// import { syncHistoryWithStore, routerReducer } from 'react-router-redux';
import createHistory from 'history/lib/createBrowserHistory';
import { IntlProvider, addLocaleData } from 'react-intl';
import ruLocaleData from 'react-intl/locale-data/ru';
import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

import createStore from './createStore';
import createRoutes from './routes';
import config from './config/config';
import utils from './lib/utils';
import WithStylesContext from './components/WithStylesContext';

import './assets/fontello/css/fontello.css';
import './style/style.css';

addLocaleData(ruLocaleData);

const
  span = document.createElement('span'),
  isBrowser = (typeof window !== 'undefined'),
  initialState = isBrowser && (window.__INITIAL_STATE__ || {}),
  locale = initialState.locale || config.fallbackLocale,
  basename = initialState.basename,
  messages = initialState.messages;

function createElement(component, props) {
  return component(props);
}

// Add the reducer to your store on the `routing` key
const store = createStore();

const history = useRouterHistory(createHistory)({
  basename,
});

function metaUpdate(meta) {
  document.title = meta.title;
  document.head.querySelector('meta[name=description]').content = meta.description;
  Array.from(document.head.querySelectorAll('link[hreflang]')).map((node) => {
    ReactDOM.unmountComponentAtNode(node);
    document.head.removeChild(node);
    return false;
  });
  const links = meta.links.reduce((acc, link) => {
    ReactDOM.render(link, span);
    return acc.concat(span.innerHTML);
  }, []);
  document.head.insertAdjacentHTML('beforeend', links.join('\n'));
}

function onUpdate() {
  const
    { routes, location } = this.state,
    meta = utils.getMeta(routes, messages, location.pathname);

  metaUpdate(meta);

  if (typeof ga !== 'undefined') {
    ga('send', 'pageview', {
      title: meta.title,
      page: location.pathname,
    });
  }
}

function onInsertCss(...styles) {
  const removeCss = styles.map(style => style._insertCss()); // eslint-disable-line no-underscore-dangle, max-len
  return () => {
    removeCss.forEach(f => f());
  };
}

match({ history, routes: createRoutes(locale, store) }, (error, redirectLocation, renderProps) => {
  ReactDOM.render(
    <Provider store={store}>
      <IntlProvider locale={locale} messages={messages}>
        <WithStylesContext onInsertCss={onInsertCss}>
          <Router
            {...renderProps}
            createElement={createElement}
            onUpdate={onUpdate}
          />
        </WithStylesContext>
      </IntlProvider>
    </Provider>,
    document.getElementById('react-view')
  );
});
