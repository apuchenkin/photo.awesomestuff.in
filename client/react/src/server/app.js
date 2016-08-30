import React from 'react';
import ReactDOM from 'react-dom/server';
import match from 'react-router/lib/match';
import RouterContext from 'react-router/lib/RouterContext';
import express from 'express';
import favicon from 'serve-favicon';
import proxy from 'http-proxy-middleware';
import { IntlProvider} from 'react-intl';
import Picker from '../components/common/langs';
import config from '../config.json';
import routes from '../routes';

const app = express();

function createElement(Component, props) {
  return <Component {...props} {...props.route.props} />;
}

function negotiateLocale(req) {
  return req.acceptsLanguages(config.locales)
  || config.fallbackLocale
  ;
}

app.use('/api', proxy({
  target: config.apiProxy,
  pathRewrite: {
    '^/api/v1' : '' // rewrite path
  },
  changeOrigin: true
}));

app.listen(3000);

app.use(favicon(__dirname + '/../assets/favicon.ico'));
app.use((req, res) => {
  const
    piece = req.url.split('/')[1],
    prefix = config.locales.find(l => l === piece),
    basename = prefix && `/${prefix}`,
    locale = prefix || negotiateLocale(req),
    location = basename ? req.url.replace(basename, '') || "/" : req.url
    ;

  // Note that req.url here should be the full URL path from
  // the original request, including the query string.
  match({ routes, location, basename }, (error, redirectLocation, renderProps) => {
    if (error) {
      res.status(500).send(error.message);
    } else if (redirectLocation) {
      res.redirect(302, redirectLocation.pathname + redirectLocation.search);
    } else if (renderProps) {
      const
        location = req.protocol + '://' + req.get('host'),
        messages = require('../translation/' + locale + '.json'),
        initialState = {
          routes: renderProps.routes,
          locale,
          basename,
          messages
        },
        componentHTML = ReactDOM.renderToString(
          <IntlProvider locale={locale} messages={messages}>
            <RouterContext {...renderProps} createElement={createElement} />
          </IntlProvider>,
        ), metaData = {
          title: 1,
          description: 1
        };

      res.status(200).send(renderHTML({
        componentHTML,
        initialState,
        metaData,
        config
      }));

      // You can also check renderProps.components or renderProps.routes for
      // your "not found" component or route respectively, and send a 404 as
      // below, if you're using a catch-all route.

    } else {
      res.status(404).send('Not found1');
    }
  });
});

//TODO: mock
const escapeHTML = x => x;

function renderHTML({ componentHTML, initialState, metaData, config }) {
  return `
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>${escapeHTML(metaData.title)}</title>
            <meta name="description" content="${escapeHTML(metaData.description)}">
            <link href='http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400' rel='stylesheet' type='text/css'>
            <link rel="stylesheet" href="${config.staticEndpoint}/bundle.css">
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
