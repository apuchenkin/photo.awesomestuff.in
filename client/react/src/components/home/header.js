import React from 'react';
import {FormattedMessage} from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import Link from '../link';

export default class Header extends React.Component {

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const title = 'PHOTO.AWESOMESTUFF.IN';

    return (
      <header className="main" ref="main">
        <h1 className="title">
          <Link to="/">{title}</Link>
          <span className="version">
            &nbsp;<FormattedMessage id="alfa" defaultMessage={`alfa`}/>
          </span>
        </h1>
        <h2 className="subtitle">
          <FormattedMessage id="description" defaultMessage={`Travel in photography`}/>
        </h2>
      </header>
    );
  }
}
