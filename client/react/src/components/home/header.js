import React from 'react';
import {FormattedMessage} from 'react-intl';
import Link from '../link';

export default class Header extends React.Component {

  render() {
    const title = 'PHOTO.AWESOMESTUFF.IN';

    return (
      <header className="main" ref="main">
        <h1 className="title">
          <Link to="/">{title}</Link>
          <span className="version">
            <FormattedMessage id="alfa" defaultMessage={`alfa`}/></span>
        </h1>
        <h2 className="subtitle">
          <FormattedMessage id="description" defaultMessage={`Travel in photography`}/>
        </h2>
      </header>
    );
  }
}
