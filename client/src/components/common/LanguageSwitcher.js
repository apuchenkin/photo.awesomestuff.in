import React from 'react';
import { connect } from 'react-redux';
// import { locationShape } from 'found/lib/PropTypes';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

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
    ? <span title={helpMsg}>{localeMsg}</span>
    : <a href={location.pathname.replace(localeURL, `/${locale}$3`)} hrefLang={locale} >{localeMsg}</a>;
};

Locale.propTypes = {
  // location: locationShape,
  locale: string.isRequired,
  disabled: bool.isRequired,
  intl: intlShape.isRequired,
};

const LanguageSwitcher = (props) => {
  const { langs, locales } = props,
    links = locales.map(locale =>
      <Locale {...props} locale={locale} disabled={!langs.find(l => locale === l)} key={locale} />
    );

  return (
    <div className={style.language}>
      {links}
    </div>
  );
};

LanguageSwitcher.propTypes = {
  langs: arrayOf(string).isRequired,
  locales: arrayOf(string).isRequired,
};

export default connect(
  state => ({ locales: state.runtime.config.locales })
)(
  withStyles(style)(injectIntl(LanguageSwitcher))
);
