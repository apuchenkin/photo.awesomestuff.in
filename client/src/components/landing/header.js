import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { connect } from 'react-redux';
import { compose } from 'redux';
import { string } from 'prop-types';
import Link from 'found/lib/Link';
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

const HomeHeader = ({ title }) => (
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

HomeHeader.propTypes = {
  title: string.isRequired,
};

export default compose(
  connect(({ runtime: { config } }) => ({
    title: config.title,
  })),
  withStyles(style),
)(HomeHeader);
