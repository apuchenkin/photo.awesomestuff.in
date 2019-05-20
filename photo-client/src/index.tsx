import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { IntlProvider, addLocaleData } from 'react-intl';
import ruLocaleData from 'react-intl/locale-data/ru';

// @ts-ignore
import createInitialBrowserRouter from 'found/lib/createInitialBrowserRouter';
import routeConfigFactory from './router/config';
import render from './router/render';

// @ts-ignore
import { queryMiddleware, createBasenameMiddleware } from 'farce';
// @ts-ignore
import StyleContext from 'isomorphic-style-loader/StyleContext';
import serviceFactory from './service/factory';

import 'normalize.css';
import 'perfect-scrollbar/css/perfect-scrollbar.css';

addLocaleData(ruLocaleData);

// @ts-ignore
const initialState = window.__INITIAL_STATE__ || {};

const insertCss = (...styles: any[]) => {
  const removeCss = styles.map(style => style._insertCss())
  return () => removeCss.forEach(dispose => dispose())
}

(async () => {
  const {
    config,
    locale = 'ru',
    basename,
    pages,
    categories,
    messages = {},
    data,
  } = initialState;

  const services = serviceFactory({
    locale,
    endpoint: config.apiEndpoint,
  });

  const routeConfig = routeConfigFactory({
    pages,
    categories,
    config,
    services,
  })

  const BrowserRouter = await createInitialBrowserRouter({
    historyMiddlewares: [
      queryMiddleware,
      createBasenameMiddleware({ basename }),
    ],
    routeConfig,
    matchContext: {
      data,
    },
    render,
  });

  ReactDOM.hydrate(
    <IntlProvider locale={locale} messages={messages}>
      <StyleContext.Provider value={{ insertCss }}>
        <BrowserRouter />
      </StyleContext.Provider>
    </IntlProvider>,
    document.getElementById('react-root'),
  );
})();
