import path from 'path';
import React from 'react';
import { Provider } from 'react-redux';
import ReactDOM from 'react-dom/server';
import { Actions as FarceActions, ServerProtocol } from 'farce';
import { getStoreRenderArgs, resolver, RedirectException } from 'found';
import { RouterProvider } from 'found/lib/server';

import express from 'express';
import { sync as globSync } from 'glob';
import { IntlProvider } from 'react-intl';
import { readFileSync } from 'fs';

import HTML from './renderHTML';
import configureStore from '../store/configure';
import { serverRender as render } from '../router/render';
import config from '../../etc/config';
import { buildMeta } from '../service/meta';
import WithStylesContext from '../components/WithStylesContext';
import serviceFactory from '../service/factory';

const translations = globSync(path.join(process.cwd(), 'src', 'translation', '*.json'))
  .map(filename => [
    path.basename(filename, '.json'),
    readFileSync(filename, 'utf8'),
  ])
  .map(([locale, file]) => [locale, JSON.parse(file)])
  .reduce((collection, [locale, messages]) =>
    Object.assign(collection, { [locale]: messages }),
  {},
  );

function catchAsyncErrors(fn) {
  return (req, res, next) => {
    const routePromise = fn(req, res, next);
    if (routePromise.catch) {
      routePromise.catch(err => next(err));
    }
  };
}

const app = express();

const negotiateLocale = req =>
  req.acceptsLanguages(config.locales)
  || config.fallbackLocale
;

if (__DEV__) {
  // const proxy = require('http-proxy-middleware');
  // const webpackDevMiddleware = require('./dev');
  // app.use(webpackDevMiddleware);
  const api = require('../../api/server');
  const statics = require('../../api/static');

  app.use('/api/v1', api);
  app.use('/static', statics);

  // app.use('/api/v1', proxy({
  //   target: 'http://photo.awesomestuff.in',
  //   changeOrigin: true,
  // }));
  // app.use('/static', proxy({
  //   target: 'http://photo.awesomestuff.in',
  //   changeOrigin: true,
  // }));
}

app.use(express.static(path.join(__dirname, 'assets')));

app.use(catchAsyncErrors(async (req, res) => {
  const piece = req.url.split('/')[1];
  const prefix = config.locales.find(l => l === piece);
  const basename = prefix && `/${prefix}`;
  const locale = prefix || negotiateLocale(req);
  const messages = locale ? translations[locale] : {};
  const css = new Set(); // CSS for all rendered React components
  // eslint-disable-next-line no-underscore-dangle
  const onInsertCss = (...styles) => styles.forEach(style => css.add(style._getCss()));
  const intlProvider = new IntlProvider({
    locale,
    messages,
  }, {});
  const { intl } = intlProvider.getChildContext();
  const { apiEndpoint } = config;

  const services = serviceFactory({
    locale,
    apiEndpoint,
  });

  const pages = await services
    .pageService
    .fetchPages()
    .catch((e) => {
      console.error(e);
      return [];
    });

  const categories = await services
    .categoryService
    .fetchCategories()
    .catch((e) => {
      console.error(e);
      return [];
    });

  const initial = {
    runtime: {
      locale,
      basename,
      messages,
      config,
    },
    page: {
      pages,
    },
    category: {
      categories,
    },
  };

  const store = await configureStore(new ServerProtocol(req.url), initial, services);
  store.dispatch(FarceActions.init());
  const matchContext = { store, intl, services };

  const renderArgs = await getStoreRenderArgs({
    store,
    matchContext,
    resolver,
  }).catch((error) => {
    if (error instanceof RedirectException) {
      res.redirect(301, store.farce.createHref(error.location));
      return;
    }

    throw error;
  });

  if (!basename) {
    res.vary('Accept-Language');
  }

  const markup = ReactDOM.renderToString(
    <Provider store={store}>
      <IntlProvider locale={locale} messages={messages}>
        <WithStylesContext onInsertCss={onInsertCss}>
          <RouterProvider router={renderArgs.router}>
            {render(renderArgs)}
          </RouterProvider>
        </WithStylesContext>
      </IntlProvider>
    </Provider>,
  );

  const meta = buildMeta(renderArgs.location, store, intl);
  const styles = [...css].join('');
  const html = ReactDOM.renderToStaticMarkup(
    <WithStylesContext onInsertCss={onInsertCss}>
      <HTML
        markup={markup}
        initialState={store.getState()}
        meta={meta}
        styles={styles}
      />
    </WithStylesContext>,
  );

  res
    .status(renderArgs.error ? renderArgs.error.status : 200)
    .send(`
      <!DOCTYPE html>
      ${html}
    `);
}));

// eslint-disable-next-line no-unused-vars
app.use((error, req, res, next) => {
  console.log(error);
  res
    .status(500)
    .send(__DEV__ ? error : 'Service unavailable');
});

const PORT = process.env.PORT || 3000;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
