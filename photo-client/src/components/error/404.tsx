import * as React from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import { defineMessages, FormattedMessage } from 'react-intl';
import Link from 'found/lib/Link';
import style from './style.scss';
import { FoundContext } from '@app/context';

const messages = defineMessages({
  title: {
    id: 'error.title',
    defaultMessage: 'Error {error}',
  },
  description: {
    id: 'error.404.description',
    defaultMessage: 'Page not found',
  },
  text: {
    id: 'error.404.text',
    defaultMessage: 'Page "{url}" that you were looking for, cannot be found.',
  },
  reasons: {
    id: 'error.404.reasons',
    defaultMessage: 'This might be because of:',
  },
  reason1: {
    id: 'error.404.reason1',
    defaultMessage: 'Page does not exists',
  },
  reason2: {
    id: 'error.404.reason2',
    defaultMessage: 'Page content is not available in current language',
  },
  contact: {
    id: 'error.404.contactText',
    defaultMessage: 'Please, {link} if you\'re followed a broken link',
  },
  contactUs: {
    id: 'error.404.contact',
    defaultMessage: 'contact us',
  },
  home: {
    id: 'error.home',
    defaultMessage: 'Return home',
  },
});

const link = (
  <Link to="/contacts" style={{ margin: 0 }}>
    <FormattedMessage {...messages.contactUs} />
  </Link>
);

interface Props {
  location: Location;
}

const NotFound: React.FunctionComponent<Props> = ({ location }) => (
  <div className={style['error-404']}>
    <h2>
      <b><FormattedMessage {...messages.title} values={{ error: 404 }} /></b>
      &nbsp;-&nbsp;<FormattedMessage {...messages.description} />
    </h2>
    <p><FormattedMessage {...messages.text} values={{ url: location.pathname }} /></p>
    <p><FormattedMessage {...messages.reasons} defaultMessage={''} /></p>
    <ol>
      <li><FormattedMessage {...messages.reason1} /></li>
      <li><FormattedMessage {...messages.reason2} /></li>
    </ol>
    <p><FormattedMessage {...messages.contact} values={{ link }} /></p>
    <Link to="/"><FormattedMessage {...messages.home} /></Link>
  </div>
);


export default withStyles(style)(NotFound);
