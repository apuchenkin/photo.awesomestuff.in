import React from 'react';
import { withRouter } from 'react-router';

const AUTH = 'auth';

class Auth extends React.Component {

  constructor() {
    super();

    this.state = {
      email: '',
      password: '',
    };
  }

  submit() {
    const auth = window.btoa([this.state.email, this.state.password].join(':'));
    localStorage.setItem(AUTH, `Basic ${auth}`);
    this.props.router.push('/');
  }

  render() {
    return (
      <div>
        <form onSubmit={this.submit}>
          Username ({this.state.email}):
          <input
            name="email"
            type="email"
            value={this.state.email}
            onChange={e => this.setState({ email: e.target.value })}
          />
          Password ({this.state.password}):
          <input
            name="password"
            type="passord"
            value={this.state.password}
            onChange={e => this.setState({ password: e.target.value })}
          />
          <input type="submit" />
        </form>
      </div>
    );
  }
}

export default withRouter(Auth);
