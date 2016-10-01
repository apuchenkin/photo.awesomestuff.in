import React from 'react';
import { Provider } from 'react-redux';
import ReactDOM from 'react-dom/server';
import match from 'react-router/lib/match';
import RouterContext from 'react-router/lib/RouterContext';
import express from 'express';
import { IntlProvider } from 'react-intl';
import path from 'path';

import HTML from './renderHTML';
import createStore from '../createStore';
import createRoutes from '../routes';
import config from '../config/config.json';
import utils from '../lib/utils';
import WithStylesContext from '../components/WithStylesContext';

const app = express();

const negotiateLocale = req =>
  req.acceptsLanguages(config.locales)
  || config.fallbackLocale
;

app.use(express.static(path.join(__dirname, 'assets')));
app.use((req, res) => {
  const
    piece = req.url.split('/')[1],
    prefix = config.locales.find(l => l === piece),
    basename = prefix && `/${prefix}`,
    locale = prefix || negotiateLocale(req),
    // eslint-disable-next-line global-require
    messages = locale ? require(`../translation/${locale}.json`) : {},
    store = createStore({ runtime: { locale, basename, messages } }),
    routes = createRoutes(store),
    location = basename ? req.url.replace(basename, '') || '/' : req.url,
    css = new Set(), // CSS for all rendered React components
    // eslint-disable-next-line no-underscore-dangle
    onInsertCss = (...styles) => styles.forEach(style => css.add(style._getCss()));

  // Note that req.url here should be the full URL path from
  // the original request, including the query string.
  match({ routes, location, basename }, (error, redirectLocation, renderProps) => {
    if (error) {
      // TODO: 503 error
      res.status(500).send(error.message);
    } else if (redirectLocation) {
      // moved permanently
      res.redirect(301, (basename || '') + redirectLocation.pathname + redirectLocation.search);
    } else if (renderProps) {
      const
        meta = utils.getMeta(renderProps.routes, messages, renderProps.location.pathname),
        componentHTML = ReactDOM.renderToString(
          <Provider store={store}>
            <IntlProvider locale={locale} messages={messages}>
              <WithStylesContext onInsertCss={onInsertCss}>
                <RouterContext {...renderProps} />
              </WithStylesContext>
            </IntlProvider>
          </Provider>
        ),
        styles = [...css].join(''),
        html = ReactDOM.renderToStaticMarkup(
          <WithStylesContext onInsertCss={onInsertCss}>
            <HTML
              componentHTML={componentHTML}
              initialState={store.getState()}
              meta={meta}
              styles={styles}
            />
          </WithStylesContext>
        );

      res.status(200).send(`<!DOCTYPE html>${html}`);

      // You can also check renderProps.components or renderProps.routes for
      // your "not found" component or route respectively, and send a 404 as
      // below, if you're using a catch-all route.
    } else {
      // TODO: 404 page should have proper status and content page
      res.status(404).send('Not found');
    }
  });
});

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
