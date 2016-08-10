import React from 'react';
import { withRouter } from 'react-router';
import classNames from 'classnames';

const AUTH = 'auth';

const Auth = withRouter(React.createClass({
  getInitialState() {
    return {email: '', password: ''};
  },

  submit() {
    localStorage.setItem(AUTH, 'Basic ' + window.btoa([this.state.email, this.state.password].join(':')));
    this.props.router.push('/');
  },

  render() {
    return (
      <div>
        <form onSubmit={this.submit}>
          Username ({this.state.email}):
          <input name="email"
            type="email"
            value={this.state.email}
            onChange={e => this.setState({email: e.target.value})}
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
}))

export default Auth;
