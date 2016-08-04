require('normalize.css/normalize.css');
require('styles/App.css');

import React from 'react'
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router'

// let yeomanImage = require('../images/yeoman.png');

const AUTH = "auth"

const App = React.createClass({
  render() {
    return (
      <div>App
        <div>{this.props.children}</div>
      </div>
    )
  }
})

const Admin = React.createClass({
  getInitialState() {
    return {
      username: localStorage.getItem(AUTH) || '',
      category: this.props.location.query ? this.props.location.query.category : null,
      categories: []
    };
  },

  componentDidMount: function() {
    let me = this,
        state = me.state;

    fetch('/api/v1/category.json')
      .then(response => {
        response.text().then(strem => {
          me.setState({categories: JSON.parse(strem)});
        })
      })

    if (state.category) {
      console.log(state.category);
    }
  },

  logout() {
    localStorage.removeItem(AUTH);
    this.props.router.push('/auth');
  },

  render() {
    let state = this.state,
        categoryNodes = state.categories.map(function(c) {
          let link = '?category=' + c.id,
            cname = state.category == c.id ? "active" : "";

          return (
            <div key={c.id}>
              <a href={link} className={cname}>{c.name}</a>
            </div>
          );
        });

    return (
      <div>Admin ({this.state.username}):
        <button onClick={this.logout}>logout</button>
        {categoryNodes}
      </div>
    )
  }
})

const checkAuth = (nextState, replace, callback) => {
  if (!localStorage.getItem(AUTH)) {
    replace('/auth');
  }
  callback();
}

const Auth = React.createClass({
  getInitialState() {
    return {username: localStorage.getItem(AUTH) || '', password: ''};
  },

  submit() {
    localStorage.setItem(AUTH, this.state.username);
    this.props.router.push('/');
  },

  render() {
    return (
      <div>
        <form onSubmit={this.submit}>
          Username ({this.state.username}):
          <input name="username"
            type="text"
            value={this.state.username}
            onChange={e => this.setState({username: e.target.value})}
          />
          Password ({this.state.password}):
          <input name="password"
            type="passord"
            value={this.state.password}
            onChange={e => this.setState({password: e.target.value})}
          />
          <input type="submit" />
        </form>
      </div>
    )
  }
})

const NoMatch = React.createClass({
  render() {
    return (
      <div>NoMatch</div>
    )
  }
})

const Users = React.createClass({
  render() {
    return (
      <div>
        <h1>Users</h1>
        <div className="master">
          <ul>
            {/* use Link to route around the app */}
            {this.state.users.map(user => (
              <li key={user.id}><Link to={`/user/${user.id}`}>{user.name}</Link></li>
            ))}
          </ul>
        </div>
        <div className="detail">
          {this.props.children}
        </div>
      </div>
    )
  }
})

const User = React.createClass({
  componentDidMount() {
    this.setState({
      // route components are rendered with useful information, like URL params
      user: findUserById(this.props.params.userId)
    })
  },

  render() {
    return (
      <div>
        <h2>{this.state.user.name}</h2>
        {/* etc. */}
      </div>
    )
  }
})

class AppComponent extends React.Component {
  render() {
    return (
      <Router history={browserHistory}>
        <Route path="/(?category=:category)" component={App}>
          <IndexRoute onEnter={checkAuth} component={withRouter(Admin)} />
          <Route path="auth" component={withRouter(Auth)} />
          {/* <Route path="users" component={Users}>
            <Route path="/user/:userId" component={User} />
          </Route> */}
          <Route path="*" component={NoMatch} />
        </Route>
      </Router>
    );
  }
}

AppComponent.defaultProps = {
};

export default AppComponent;
