import React                     from 'react';
import ReactDOM                  from 'react-dom/server';
import { match, Router, RouterContext } from 'react-router';
import express      from 'express';
import favicon from 'serve-favicon';
import Promise from 'promise';
import routes from './routes';

const app = express();

class ExtraDataProvider extends React.Component {
  getChildContext() {
    return { initialState: this.initialState }
  }

  constructor(props, context) {
    super(props, context)
    this.initialState = props.initialState
  }

  render() {
    return React.Children.only(this.props.children)
  }
}
ExtraDataProvider.childContextTypes = {
  initialState: React.PropTypes.any.isRequired
};

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

      let fetchers = renderProps.components.filter(c => !!c.fetchData)
      const promises = fetchers
        .map(f => f.fetchData('http://photo.awesomestuff.in'))
        .reduce((obj, p) => Object.assign(obj, p), {});

      Promise.all(Object.keys(promises).map(p => promises[p])).then(data => {
        // let state = Object.keys(promises).reduce((obj, p) => obj[p] = , {});
          let state = Object.keys(promises).reduce((o,p) => {o[p] = data[o.i]; return o}, {i: 0}); //todo: refactor this shit
          let html = ReactDOM.renderToString(<ExtraDataProvider initialState={state}><RouterContext {...renderProps} /></ExtraDataProvider>);
          res.status(200).send(`<!DOCTYPE html><html><body>${html}</body></html>`)
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

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
    console.log(`Server listening on: ${PORT}`);
});
