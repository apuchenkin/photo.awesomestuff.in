import React from 'react';
import { locationShape } from 'react-router/lib/PropTypes';
import { injectIntl, intlShape } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import { locales } from '../../config/config.json';
import style from './style.less';

const { arrayOf, string } = React.PropTypes;

class Picker extends React.Component {

  static propTypes = {
    location: locationShape,
    langs: arrayOf(string).isRequired,
    intl: intlShape.isRequired,
  }

  static localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const { location, langs, intl } = this.props,
      links = locales.map((locale) => {
        const
          disabled = !langs.find(l => locale === l),
          localeMsg = intl.formatMessage({ id: locale, defaultMessage: locale }),
          helpMsg = intl.formatMessage({ id: 'locale.not_available', defaultMessage: 'Current page content is not available in language {lang} yet' }, { lang: localeMsg });

        return disabled
          ? <span key={locale} title={helpMsg}>{localeMsg}</span>
          : <a key={locale} href={location.pathname.replace(Picker.localeURL, `/${locale}$3`)} hrefLang={locale} >{localeMsg}</a>;
      });

    return (
      <div className={style.language}>
        {links}
      </div>
    );
  }
}

export default withStyles(style)(injectIntl(Picker));
