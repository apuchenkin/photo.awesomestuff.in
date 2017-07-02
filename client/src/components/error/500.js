import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import baseStyle from '../../style/style.less';
import style from './style.less';

const messages = defineMessages({
  title: {
    id: 'error.title',
    defaultMessage: 'Error {error}',
  },
  description: {
    id: 'error.500.description',
    defaultMessage: 'Service unavailable',
  },
  text: {
    id: 'error.500.text',
    defaultMessage: 'Service temporary unavailable. Please come back later',
  },
});

const ServiceUnavailable = () => (
  <div className={style['error-404']}>
    <h2>
      <b><FormattedMessage {...messages.title} values={{ error: 500 }} /></b>
      &nbsp;-&nbsp;<FormattedMessage {...messages.description} />
    </h2>
    <p><FormattedMessage {...messages.text} /></p>
  </div>
);

export default withStyles(style, baseStyle)(ServiceUnavailable);
