import React from 'react';
import { locationShape } from 'react-router/lib/PropTypes';
import { FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import Link from '../link';

export default class Header extends React.Component {

  static propTypes = {
    location: locationShape,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const { location } = this.props;

    return (
      <div className="error-404">
        <h2>
          <b><FormattedMessage id="error.title" defaultMessage={'Error {error}'} values={{ error: 404 }} /></b>
          &nbsp;-&nbsp;<FormattedMessage id="error.404.description" defaultMessage={'Page not found'} />
        </h2>
        <p><FormattedMessage id="error.404.text" defaultMessage={'Page "{url}" that you were looking for, cannot be found.'} values={{ url: location.pathname }} /></p>
        <p><FormattedMessage id="error.404.reasons" defaultMessage={'This might be because of:'} /></p>
        <ol>
          <li><FormattedMessage id="error.404.reason1" defaultMessage={'Page does not exists'} /></li>
          <li><FormattedMessage id="error.404.reason2" defaultMessage={'Page content is not available in current language'} /></li>
        </ol>
        <p><FormattedMessage
          id="error.404.contactText"
          defaultMessage={'Please, {link} if you\'re followed a broken link'}
          values={{ link: <Link to="/contacts" style={{ margin: 0 }}><FormattedMessage id="error.404.contact" defaultMessage={'contact us'} /></Link> }}
        /></p>
        <Link to="/"><FormattedMessage id="error.home" defaultMessage={'Return home'} /></Link>
      </div>
    );
  }
}
