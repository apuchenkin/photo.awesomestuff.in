import React from 'react';
import ReactDOM from 'react-dom/server';
import match from 'react-router/lib/match';
import RouterContext from 'react-router/lib/RouterContext';
import express from 'express';
import favicon from 'serve-favicon';
import proxy from 'http-proxy-middleware';
import { IntlProvider } from 'react-intl';
import escapeHtml from 'escape-html';

import Picker from '../components/common/langs';
import createRoutes from '../routes';
import config from '../config.json';
import utils from '../lib/utils';

const app = express();
const createElement = (component, props) => component(props);

function negotiateLocale(req) {
  return req.acceptsLanguages(config.locales)
  || config.fallbackLocale
  ;
}

app.use('/api', proxy({
  target: config.apiProxy,
  pathRewrite: {
    '^/api/v1' : '' // rewrite path
  }
}));

app.listen(3000);

app.use(favicon(__dirname + '/../assets/favicon.ico'));
app.use((req, res) => {
  const
    piece = req.url.split('/')[1],
    prefix = config.locales.find(l => l === piece),
    basename = prefix && `/${prefix}`,
    locale = prefix || negotiateLocale(req),
    messages = require('../translation/' + locale + '.json'),
    routes = createRoutes(locale, messages),
    location = basename ? req.url.replace(basename, '') || "/" : req.url
    ;

  // Note that req.url here should be the full URL path from
  // the original request, including the query string.
  match({ routes, location, basename }, (error, redirectLocation, renderProps) => {
    if (error) {
      //TODO: 503 error
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
          messages
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
        config
      }));

      // You can also check renderProps.components or renderProps.routes for
      // your "not found" component or route respectively, and send a 404 as
      // below, if you're using a catch-all route.

    } else {
      //TODO: 404 page should have proper status and content page
      res.status(404).send('Not found1');
    }
  });
});

function renderHTML({ componentHTML, initialState, meta, config }) {
  return `
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width initial-scale=1.0">
        <title>${escapeHtml(meta.title)}</title>
        <meta name="description" content="${escapeHtml(meta.description)}">
        <link href='http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400' rel='stylesheet' type='text/css'>
        <link rel="stylesheet" href="${config.staticEndpoint}/bundle.css">
        ${meta.links.join("\n")}
    </head>
    <body>
      <div id="react-view" class="wrapper">${componentHTML}</div>
      <script type="application/javascript">
        window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};
      </script>
      <script type="application/javascript" src="${config.staticEndpoint}/bundle.js"></script>
    </body>
    </html>
  `;
}

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
  console.log(`Server listening on: ${PORT}`);
});
