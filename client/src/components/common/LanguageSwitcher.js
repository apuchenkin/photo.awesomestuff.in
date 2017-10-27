import React from 'react';
import { shape, arrayOf, string, bool } from 'prop-types';
import { connect } from 'react-redux';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import style from './style.less';
import Links from './links';

const messages = defineMessages({
  not_available: {
    id: 'locale.not_available',
    defaultMessage: 'Current page content is not available in language {lang} yet',
  },
});

const Locale = ({ location, locale, intl, disabled }) => {
  const localeMsg = intl.formatMessage({ id: locale, defaultMessage: locale });
  const title = intl.formatMessage(messages.not_available, { lang: localeMsg });

  return disabled
    ? <span title={title}>{localeMsg}</span>
    : <a href={`/${locale}${location.pathname}`} hrefLang={locale} >{localeMsg}</a>;
};

Locale.propTypes = {
  location: shape({
    pathname: string.isRequired,
  }).isRequired,
  locale: string.isRequired,
  disabled: bool.isRequired,
  intl: intlShape.isRequired,
};

const LanguageSwitcher = ({ langs, locales, location, ...props }) => (
  <div className={style.language}>
    {locales.map(locale => (
      <Locale
        location={location}
        locale={locale}
        disabled={!langs.find(l => locale === l)}
        key={locale}
        {...props}
      />
    ))}
    <Links langs={langs} url={location.pathname} />
  </div>
);

LanguageSwitcher.propTypes = {
  langs: arrayOf(string).isRequired,
  locales: arrayOf(string).isRequired,
};

export default connect(
  ({ runtime: { config }, found: { resolvedMatch } }, { langs }) => ({
    locales: config.locales,
    location: resolvedMatch.location,
    langs: langs || config.locales,
  }),
)(
  withStyles(style)(injectIntl(LanguageSwitcher)),
);
