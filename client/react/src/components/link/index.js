import React from 'react';
import Link from 'react-router/lib/Link';
import utils from '../../lib/utils';

const { string, func } = React.PropTypes;

export default class LocaleLink extends Link {

  static contextTypes = {
    locale: string
  }

  render() {
    const
      locale = this.context.locale,
      to = this.props.to
      ;
    return <Link {...this.props} to={locale ? `/${locale}${to}` : to} />;
  }
}
