import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from '../link';
import style from '../../style/header.less';

const { string, shape } = React.PropTypes;

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

class PageHeader extends React.Component {

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
          <Link to="/" activeClassName="active">
            <FormattedMessage {...messages.home} />
          </Link> / {this.props.page.title}
        </h1>
      </header>
    );
  }
}

export default withStyles(style)(PageHeader);
