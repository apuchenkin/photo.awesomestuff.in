import React from 'react';
import {FormattedMessage} from 'react-intl';
import Link from '../link';

export default class Header extends React.Component {
  //TODO: backURL
  render() {
    return (
      <div className="error-404">
        <h2>
          <b>
          <FormattedMessage id="error.title" defaultMessage={`Error {error}`} values={{error: 404}}/></b>
          &nbsp;-&nbsp;<FormattedMessage id="error.404.description" defaultMessage={`Page not found`}/>
        </h2>
        <a href="/ru"><FormattedMessage id="error.home" defaultMessage={`Return home`}/></a>
      </div>
    );
  }
}
