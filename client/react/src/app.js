import React                     from 'react';
import ReactDOM                  from 'react-dom/server';
import { match, Router, RouterContext } from 'react-router';
import express      from 'express';

import App from './components/app';
import routes from './routes';

const app = express();

app.use((req, res) => {
  // Note that req.url here should be the full URL path from
  // the original request, including the query string.
  match({ routes, location: req.url }, (error, redirectLocation, renderProps) => {
    if (error) {
      res.status(500).send(error.message)
    } else if (redirectLocation) {
      res.redirect(302, redirectLocation.pathname + redirectLocation.search)
    } else if (renderProps) {
      // You can also check renderProps.components or renderProps.routes for
      // your "not found" component or route respectively, and send a 404 as
      // below, if you're using a catch-all route.
      let html = ReactDOM.renderToString(<RouterContext {...renderProps} />);
      res.status(200).send(`<!DOCTYPE html><html><body>${html}</body></html>`)
    } else {
      res.status(404).send('Not found1')
    }
  })
})

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
    console.log(`Server listening on: ${PORT}`);
});
