import * as React from 'react';
import { AuthContext } from '@app/context';

const Auth: React.FunctionComponent = () => {
  const emailRef = React.useRef(null);
  const passwordRef = React.useRef(null);

  const { login } = React.useContext(AuthContext);

  const submit = () => {
    const email = emailRef.current.value;
    const password = passwordRef.current.value;

    login(email, password);
  }

  return (
    <div className="auth">
      <form onSubmit={submit}>
        Username:
        <input
          ref={emailRef}
          name="email"
          type="email"
        />
        Password:
        <input
          ref={passwordRef}
          name="password"
          type="passord"
        />
        <input type="submit" />
      </form>
    </div>
  );
}

export default Auth;