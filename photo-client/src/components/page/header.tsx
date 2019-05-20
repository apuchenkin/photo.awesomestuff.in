import React, { FunctionComponent } from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Link from 'found/lib/Link';

import style from './header.scss';

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

interface Props {
  title: string
}

const PageHeader: FunctionComponent<Props> = ({ title }) => (
  <header className={style.main}>
    <h1 className={style.title}>
      <Link to="/" activeClassName="active">
        <FormattedMessage {...messages.home} />
      </Link> / {title}
    </h1>
  </header>
);

export default withStyles(style)(PageHeader);
