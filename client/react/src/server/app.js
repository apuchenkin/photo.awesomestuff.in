import React from 'react';
import ReactDOM from 'react-dom/server';
import match from 'react-router/lib/match';
import RouterContext from 'react-router/lib/RouterContext';
import express from 'express';
import favicon from 'serve-favicon';
import { IntlProvider } from 'react-intl';
import path from 'path';

import renderHTML from './renderHTML';
import createRoutes from '../routes';
import config from '../config/config.json';
import utils from '../lib/utils';

import icon from '../assets/favicon.ico';

const app = express();
const createElement = (component, props) => component(props);

function negotiateLocale(req) {
  return req.acceptsLanguages(config.locales)
  || config.fallbackLocale
  ;
}

app.use(favicon(icon));
app.use((req, res) => {
  const
    piece = req.url.split('/')[1],
    prefix = config.locales.find(l => l === piece),
    basename = prefix && `/${prefix}`,
    locale = prefix || negotiateLocale(req),
    // eslint-disable-next-line global-require
    messages = locale ? require(`../translation/${locale}.json`) : {},
    routes = createRoutes(locale, messages),
    location = basename ? req.url.replace(basename, '') || '/' : req.url
    ;

  // Note that req.url here should be the full URL path from
  // the original request, including the query string.
  match({ routes, location, basename }, (error, redirectLocation, renderProps) => {
    if (error) {
      // TODO: 503 error
      res.status(500).send(error.message);
    } else if (redirectLocation) {
      // moved permanently
      res.redirect(301, basename + redirectLocation.pathname + redirectLocation.search);
    } else if (renderProps) {
      const
        initialState = {
          routes: renderProps.routes,
          locale,
          basename,
          messages,
        },
        meta = utils.getMeta(renderProps.routes, messages, renderProps.location.pathname),
        componentHTML = ReactDOM.renderToString(
          <IntlProvider locale={locale} messages={messages}>
            <RouterContext {...renderProps} createElement={createElement} />
          </IntlProvider>,
        );

      res.status(200).send(renderHTML({
        componentHTML,
        initialState,
        meta,
        config,
      }));

      // You can also check renderProps.components or renderProps.routes for
      // your "not found" component or route respectively, and send a 404 as
      // below, if you're using a catch-all route.
    } else {
      // TODO: 404 page should have proper status and content page
      res.status(404).send('Not found1');
    }
  });
});

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
