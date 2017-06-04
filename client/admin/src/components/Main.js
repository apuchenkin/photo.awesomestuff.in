// require('normalize.css/normalize.css');

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
import Upload from './Upload';
import Header from './Header';

require('../../../src/style/style.less');
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
    <div>
      <Header />
      <Switch>
        <Route exact path="/" component={App} />
        <Route path="/auth" component={Auth} />
        <Route path="/upload" component={Upload} />
        <Route component={NoMatch} />
      </Switch>
    </div>


    {/* <Route path="/" component={App} >
      <IndexRoute onEnter={checkAuth} component={Admin} />
      <Route path="auth" component={Auth} />
      <Route path="category/:category" component={Admin} />
      <Route path="*" component={NoMatch} />
    </Route> */}
  </Router>
);

export default DragDropContext(HTML5Backend)(AppComponent);
