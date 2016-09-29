import React from 'react';
import { locationShape } from 'react-router/lib/PropTypes';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import { locales } from '../../config/config.json';
import { localeURL } from '../../lib/utils';
import style from './style.less';

const { arrayOf, string, bool } = React.PropTypes;

const messages = defineMessages({
  not_available: {
    id: 'locale.not_available',
    defaultMessage: 'Current page content is not available in language {lang} yet',
  },
});

const Locale = (props) => {
  const
    { location, locale, intl, disabled } = props,
    localeMsg = intl.formatMessage({ id: locale, defaultMessage: locale }),
    helpMsg = intl.formatMessage(messages.not_available, { lang: localeMsg });

  return disabled
    ? <span key={locale} title={helpMsg}>{localeMsg}</span>
    : <a key={locale} href={location.pathname.replace(localeURL, `/${locale}$3`)} hrefLang={locale} >{localeMsg}</a>;
};

Locale.propTypes = {
  location: locationShape,
  locale: string.isRequired,
  disabled: bool.isRequired,
  intl: intlShape.isRequired,
};

const LanguageSwitcher = (props) => {
  const { langs } = props,
    links = locales.map(locale =>
      <Locale {...props} locale={locale} disabled={!langs.find(l => locale === l)} />
    );

  return (
    <div className={style.language}>
      {links}
    </div>
  );
};

LanguageSwitcher.propTypes = {
  langs: arrayOf(string).isRequired,
};

export default withStyles(style)(injectIntl(LanguageSwitcher));
