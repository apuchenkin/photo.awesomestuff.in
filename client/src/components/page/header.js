import React from 'react';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Link from 'found/lib/Link';

import style from '../../style/header.less';

const { string, shape } = React.PropTypes;

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

const PageHeader = ({ page }) => (
  <header className={style.main}>
    <h1 className={style.title}>
      <Link to="/" activeClassName="active">
        <FormattedMessage {...messages.home} />
      </Link> / {page.title}
    </h1>
  </header>
);

PageHeader.propTypes = {
  page: shape({ title: string.isRequired }).isRequired,
};

export default connect(
  ({ page: { page } }) => ({ page }),
)(
  withStyles(style)(PageHeader),
);
