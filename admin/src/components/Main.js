import React from 'react';
import {
  BrowserRouter as Router,
  Route,
  Switch,
} from 'react-router-dom';

import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import Auth from './Auth';
import App from './App';
import Header from './Header';

require('../../../client/src/style/style.less');
require('../styles/app.less');

// const checkAuth = (nextState, replace, callback) => {
//   if (!localStorage.getItem(AUTH)) {
//     replace('/auth');
//   }
//   callback();
// };

const NoMatch = () => <div>NoMatch</div>;

const AppComponent = () => (
  <Router>
    <div id="app">
      <Header />
      <Switch>
        <Route exact path="/" component={App} />
        <Route path="/category/:category" component={App} />
        <Route path="/auth" component={Auth} />
        <Route component={NoMatch} />
      </Switch>
    </div>
  </Router>
);

export default DragDropContext(HTML5Backend)(AppComponent);
