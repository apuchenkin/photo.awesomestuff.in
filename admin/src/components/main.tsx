import * as React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import Home from './home';
import Header from './header';

const NoMatch = () => <div>NoMatch</div>;

const Main = () => (
  <main id="app">
    <Header />
    <Switch>
      <Route path="/" component={Home} />
      <Route component={NoMatch} />
    </Switch>
  </main>
)

export default Main;