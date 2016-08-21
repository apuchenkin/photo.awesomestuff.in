import React                     from 'react';
import ReactDOM                  from 'react-dom/server';
import { match, Router, RouterContext } from 'react-router';
import express      from 'express';
import favicon from 'serve-favicon';
import Promise from 'promise';
import routes from './routes';
import proxy from 'http-proxy-middleware';
import ExtraDataProvider from './components/provider.js';
import config from './config.json';

const app = express();

app.use('/api', proxy({
  target: config.apiEndpoint,
  pathRewrite: {
    '^/api/v1' : '', // rewrite path
  },
  changeOrigin: true
}));

app.listen(3000);

app.use(favicon(__dirname + '/assets/favicon.ico'));
app.use((req, res) => {
  console.log(req.url);
  // Note that req.url here should be the full URL path from
  // the original request, including the query string.
  match({ routes, location: req.url }, (error, redirectLocation, renderProps) => {
    if (error) {
      res.status(500).send(error.message)

    } else if (redirectLocation) {
      res.redirect(302, redirectLocation.pathname + redirectLocation.search)
    } else if (renderProps) {
      let location = req.protocol + '://' + req.get('host');
      let fetchers = renderProps.components.filter(c => !!c.fetchData)

      const promises = fetchers
        .map(f => f.fetchData(location, renderProps.params))
        .reduce((obj, p) => Object.assign(obj, p), {});

      Promise.all(Object.keys(promises).map(p => promises[p])).then(data => {
        // let state = Object.keys(promises).reduce((obj, p) => obj[p] = , {});
          let initialState = Object.keys(promises).reduce((o,p) => {o[p] = data[o.i++]; return o}, {i: 0}); //todo: refactor this shit

          let componentHTML = ReactDOM.renderToString(
            <ExtraDataProvider initialState={initialState}>
              <RouterContext {...renderProps} />
            </ExtraDataProvider>
          );

          let metaData = {
            title: 1,
            description: 1
          }

          let clientConfig = {
            "staticUrl": "http://localhost:8080",
          }

          res.status(200).send(renderHTML({
              componentHTML,
              initialState,
              metaData,
              config : clientConfig
          }))
        }).catch(e => console.log(e))
        ;

      // You can also check renderProps.components or renderProps.routes for
      // your "not found" component or route respectively, and send a 404 as
      // below, if you're using a catch-all route.

    } else {
      res.status(404).send('Not found1')
    }
  })
})

//todo: mock
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
            <link rel="stylesheet" href="${config.staticUrl}/bundle.css">
        </head>
        <body>
          <div id="react-view">${componentHTML}</div>
          <script type="application/javascript">
            window.__CONFIG__ = ${JSON.stringify(config)};
            window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};
          </script>
          <script type="application/javascript" src="${config.staticUrl}/bundle.js"></script>
        </body>
        </html>
    `;
}

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
    console.log(`Server listening on: ${PORT}`);
});
