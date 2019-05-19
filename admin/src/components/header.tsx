import * as React from 'react';
import { AuthContext } from '@app/context';

export default () => {
  const { logout } = React.useContext(AuthContext);

  return (
    <header className="main">
      <h1>PHOTO.AWESOMESTUFF.IN</h1>
      <div className="tools">
        <button onClick={logout}>Logout</button>
      </div>
    </header>
  )
}
