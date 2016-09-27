import React from 'react';
import { locationShape } from 'react-router/lib/PropTypes';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import { locales } from '../../config/config.json';
import { localeURL } from '../../lib/utils';
import style from './style.less';

const { arrayOf, string } = React.PropTypes;

const messages = defineMessages({
  not_available: {
    id: 'locale.not_available',
    defaultMessage: 'Current page content is not available in language {lang} yet',
  },
});

const LanguageSwitcher = (props) => {
  const { location, langs, intl } = props,
    links = locales.map((locale) => {
      const
        disabled = !langs.find(l => locale === l),
        localeMsg = intl.formatMessage({ id: locale, defaultMessage: locale }),
        helpMsg = intl.formatMessage(messages.not_available, { lang: localeMsg });

      return disabled
        ? <span key={locale} title={helpMsg}>{localeMsg}</span>
        : <a key={locale} href={location.pathname.replace(localeURL, `/${locale}$3`)} hrefLang={locale} >{localeMsg}</a>;
    });

  return (
    <div className={style.language}>
      {links}
    </div>
  );
};

LanguageSwitcher.propTypes = {
  location: locationShape,
  langs: arrayOf(string).isRequired,
  intl: intlShape.isRequired,
};

export default withStyles(style)(injectIntl(LanguageSwitcher));
