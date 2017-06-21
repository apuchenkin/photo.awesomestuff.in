import React from 'react';
import { withRouter } from 'react-router';
import AuthService from '../service/auth';

class Auth extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      email: '',
      password: '',
    };

    this.submit = this.submit.bind(this);
  }

  submit() {
    const { email, password } = this.state;
    const { history } = this.props;

    AuthService.login(email, password).then(() => {
      history.push('/');
    });
  }

  render() {
    return (
      <div className="auth">
        <form onSubmit={this.submit}>
          Username:
          <input
            name="email"
            type="email"
            value={this.state.email}
            onChange={e => this.setState({ email: e.target.value })}
          />
          Password:
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
