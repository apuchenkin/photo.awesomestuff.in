import React from 'react';
import { FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from '../link';
import { title } from '../../config/config';
import style from '../../style/header.less';

class Header extends React.Component {

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    return (
      <header className={style.main}>
        <h1 className={style.title}>
          <Link to="/">{title}</Link>
          <span className={style.version}>
            &nbsp;<FormattedMessage id="alfa" defaultMessage={'alfa'} />
          </span>
        </h1>
        <h2 className={style.subtitle}>
          <FormattedMessage id="description" defaultMessage={'Travel in photography'} />
        </h2>
      </header>
    );
  }
}

export default withStyles(style)(Header);
