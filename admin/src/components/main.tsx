import * as React from 'react';
import {
  Route,
  Switch,
  // Redirect,
  // withRouter,
} from 'react-router-dom';
// import { connect } from 'react-redux';

// import Auth from './Auth';
import Home from './home';
import Header from './header';
// import authService from '../service/auth';
// import { init } from '../store/runtime/actions';

// require('../../../client/src/style/style.less');
// require('../styles/app.less');

const NoMatch = () => <div>NoMatch</div>;

const Main = () => (
  <main id="app">
    <Header />
    <Switch>
      <Route exact path="/" component={Home} />
      {/* <Route path="/category/:category" component={App} /> */}
      <Route component={NoMatch} />
    </Switch>
  </main>
)

export default Main;

// {
//   componentWillMount() {
//     this.props.initServices(authService.getToken());
//   }

//   render() {
//     return (

//     );
//   }
// }

// const Main = withRouter(AppComponent);
// const AuthWrapper = ({ initServices }) => (
//   authService.getToken()
//     ? <Main initServices={initServices} />
//     : (
//       <Switch>
//         <Route exact path="/auth" component={Auth} />
//         <Redirect to="/auth" />
//       </Switch>
//     )
// );
