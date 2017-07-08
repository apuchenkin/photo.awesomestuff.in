import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from 'found/lib/Link';
import { title } from '../../etc/config';
import style from '../../style/header.less';

const messages = defineMessages({
  alfa: {
    id: 'alfa',
    defaultMessage: 'alfa',
  },
  description: {
    id: 'description',
    defaultMessage: 'Travel in photography',
  },
});

const HomeHeader = () => (
  <header className={style.main}>
    <h1 className={style.title}>
      <Link to="/">{title}</Link>
      <span className={style.version}>
        &nbsp;<FormattedMessage {...messages.alfa} />
      </span>
    </h1>
    <h2 className={style.subtitle}>
      <FormattedMessage {...messages.description} />
    </h2>
  </header>
);

export default withStyles(style)(HomeHeader);
