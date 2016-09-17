import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import { FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from '../link';
import style from '../../style/header.less';

const { string, shape } = React.PropTypes;

class Header extends React.Component {

  static propTypes = {
    page: shape({ title: string.isRequired }).isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    return (
      <header className={style.main}>
        <h1 className={style.title}>
          <Link to="/" activeClassName="active"><FormattedMessage
            id="home"
            defaultMessage={'Home'}
          /></Link> / {this.props.page.title}
        </h1>
      </header>
    );
  }
}

export default withStyles(style)(Header);
