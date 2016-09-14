import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import { FormattedMessage } from 'react-intl';
import Link from '../link';

const { string, shape } = React.PropTypes;

export default class Header extends React.Component {

  static propTypes = {
    page: shape({ title: string.isRequired }).isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    return (
      <header className="main">
        <h1 className="title">
          <Link to="/" activeClassName="active"><FormattedMessage
            id="home"
            defaultMessage={'Home'}
          /></Link> / {this.props.page.title}
        </h1>
      </header>
    );
  }
}
