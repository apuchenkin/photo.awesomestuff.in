import React                     from 'react';
import { Route, Redirect, createRoutes, withRouter, IndexRoute } from 'react-router';

import Home from './components/home';
import Gallery from './components/gallery';

class App extends React.Component {
    render() {
      return (
          <div id='app-view'>
              {this.props.children}
          </div>
      );
    }
}

class NoMatch extends React.Component {
  render() {
    return (
      <div>NoMatch</div>
    )
  }
}

export default (
  // <Route component={withRouter(App)}>
    // <Redirect from='' to='/' />
    <Route path="/" component={App} > //(:locale)
      <IndexRoute component={Home} />
      <Route path=":category(/:subcategory)" component={Gallery} />
    </Route>
  //   <Route path="*" component={NoMatch} />
  // </Route>
);
