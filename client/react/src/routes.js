import React                     from 'react';
import { Route, Redirect, createRoutes } from 'react-router';

import Home from './components/home';
import Profile from './components/profile';

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
  <Route component={App}>
    <Redirect from='' to='/' />
    <Route path="/:locale" component={Home} >
      <Route path=":category(/:subcategory)" component={Profile} />
    </Route>
    <Route path="*" component={NoMatch} />
  </Route>
);
