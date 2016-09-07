import React from 'react';
import {locales} from '../../config.json';
import {locationShape} from 'react-router/lib/PropTypes';
import {FormattedMessage} from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
const {array} = React.PropTypes;

class Picker extends React.Component {

  static propTypes = {
    location: locationShape,
    langs: array.isRequired
  }

  static localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      {location, langs} = this.props,
      href = locale => location.pathname.replace(Picker.localeURL, `/${locale}$3`),
      isDisabled = locale => !langs.find(l => locale === l),
      links = locales.map(locale => (
        <a href={href(locale)} disabled={isDisabled(locale)} hrefLang={locale} key={locale}>
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
