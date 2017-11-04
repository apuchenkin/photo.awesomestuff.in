import React from 'react';
import { string } from 'prop-types';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Link from 'found/lib/Link';

import style from '../../style/header.less';

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

const PageHeader = ({ title }) => (
  <header className={style.main}>
    <h1 className={style.title}>
      <Link to="/" activeClassName="active">
        <FormattedMessage {...messages.home} />
      </Link> / {title}
    </h1>
  </header>
);

PageHeader.propTypes = {
  title: string.isRequired,
};

export default withStyles(style)(PageHeader);
