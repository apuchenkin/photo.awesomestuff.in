import React from 'react';
import {locales} from '../../config.json';
import {locationShape} from 'react-router/lib/PropTypes';
import {FormattedMessage} from 'react-intl';

const {array} = React.PropTypes;

class Picker extends React.Component {

  static propTypes = {
    location: locationShape,
    langs: array.isRequired
  }

  static localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g

  render() {
    const
      {location, langs} = this.props,
      links = locales.map(locale => (
        <a href={location.pathname.replace(Picker.localeURL, `/${locale}$3`)} disabled={!langs.find(l => locale === l)} hrefLang={locale} key={locale}>
          <FormattedMessage id={locale} defaultMessage={locale} />
        </a>
      ));

    //TODO: determine is the page available
    return (
      <div className="language">
        {links}
      </div>
    );
  }
}

export default Picker;
