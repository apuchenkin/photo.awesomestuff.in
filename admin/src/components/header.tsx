import * as React from 'react';
import { withRouter } from 'react-router-dom';
// import AuthService from '../service/auth';

// const logout = history => () => {
//   AuthService.logout().then(() => history.push('/auth'));
// };

export default withRouter(({ history }) => (
  <header className="main">
    <h1>PHOTO.AWESOMESTUFF.IN</h1>
    {/* <div className="tools">
      <button onClick={logout(history)}>Logout</button>
    </div> */}
  </header>
));
