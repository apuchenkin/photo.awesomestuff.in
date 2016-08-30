import React from 'react';
import Link from 'react-router/lib/Link';
import utils from '../../lib/utils';

const { string, func } = React.PropTypes;

export default Link;
// export default class LocaleLink extends Link {
//
//   // static contextTypes = {
//   //   prefix: string
//   // }
//   //
//   // render() {
//   //   const
//   //     prefix = this.context.prefix,
//   //     to = this.props.to === '/' ? '' : this.props.to;
//   //
//   //   return <Link {...this.props} to={prefix ? `/${prefix}${to}` : to} />;
//   // }
// }
