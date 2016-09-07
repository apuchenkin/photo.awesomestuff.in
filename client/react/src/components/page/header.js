import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import {FormattedMessage} from 'react-intl';
import Link from '../link';

const { object } = React.PropTypes;

export default class Header extends React.Component {

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }
  
  static propTypes = {
    page: object.isRequired
  }

  render() {
    return (
      <header className="main" ref="main">
        <h1 className="title">
          <Link to='/' activeClassName="active"><FormattedMessage
            id="home"
            defaultMessage={`Home`}
            /></Link> / {this.props.page.title}
        </h1>
      </header>
    );
  }
}
