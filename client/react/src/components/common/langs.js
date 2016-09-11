import React from 'react';
import {locales} from '../../config.json';
import {locationShape} from 'react-router/lib/PropTypes';
import {injectIntl, intlShape} from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
const {array} = React.PropTypes;

class Picker extends React.Component {

  static propTypes = {
    location: locationShape,
    langs: array.isRequired,
    intl: intlShape.isRequired
  }

  static localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      {location, langs, intl} = this.props,
      links = locales.map(locale => {
        const
          disabled = !langs.find(l => locale === l),
          localeMsg = intl.formatMessage({id: locale, defaultMessage: locale}),
          helpMsg = intl.formatMessage({id: 'locale.not_available', defaultMessage: "Current page content is not available in language {lang} yet"}, {lang: localeMsg});

        return disabled
          ? <span key={locale} title={helpMsg}>{localeMsg}</span>
          : <a key={locale} href={location.pathname.replace(Picker.localeURL, `/${locale}$3`)} hrefLang={locale} key={locale}>{localeMsg}</a>;
      });

    //TODO: determine is the page available
    return (
      <div className="language">
        {links}
      </div>
    );
  }
}

export default injectIntl(Picker);
