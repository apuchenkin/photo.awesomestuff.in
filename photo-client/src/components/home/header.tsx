import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Link from 'found/lib/Link';
import { ConfigContext } from '@app/context';
import style from '@app/components/page/header.scss';

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

const HomeHeader = () => {
  const { title } = React.useContext(ConfigContext);

  return (
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
  )
};

export default withStyles(style)(HomeHeader);
