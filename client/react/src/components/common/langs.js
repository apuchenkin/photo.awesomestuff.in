import React from 'react';
import {locales} from '../../config.json';
import {locationShape} from 'react-router/lib/PropTypes';

const { object } = React.PropTypes;

class Picker extends React.Component {

  static propTypes = {
    location: locationShape
  }

  static localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g

  render() {
    const
      {pathname} = this.props.location,
      links = locales.map(locale => (
        <a href={pathname.replace(Picker.localeURL, `/${locale}$3`)} hrefLang={locale} key={locale}>{locale}</a>
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
