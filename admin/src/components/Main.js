import React from 'react';
import {
  Route,
  Switch,
  Redirect,
  withRouter,
} from 'react-router-dom';
import { connect } from 'react-redux';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import Auth from './Auth';
import App from './App';
import Header from './Header';
import authService from '../service/auth';
import { init } from '../store/runtime/actions';

require('../../../client/src/style/style.less');
require('../styles/app.less');

const NoMatch = () => <div>NoMatch</div>;

class AppComponent extends React.PureComponent {
  componentWillMount() {
    this.props.initServices(authService.getToken());
  }

  render() {
    return (
      <div id="app">
        <Header />
        <Switch>
          <Route exact path="/" component={App} />
          <Route path="/category/:category" component={App} />
          <Route component={NoMatch} />
        </Switch>
      </div>
    );
  }
}

const Main = withRouter(AppComponent);
const AuthWrapper = ({ initServices }) => (
  authService.getToken()
    ? <Main initServices={initServices} />
    : (
      <Switch>
        <Route exact path="/auth" component={Auth} />
        <Redirect to="/auth" />
      </Switch>
    )
);

export default withRouter(connect(
  null,
  dispatch => ({
    initServices: token => dispatch(init(token)),
  }),
)(DragDropContext(HTML5Backend)(AuthWrapper)));
