import React from 'react';
import Link from './index';
import utils from '../../lib/utils';

const { string, number } = React.PropTypes;

export default class PhotoLink extends React.Component {

  static propTypes = {
    photoId: number.isRequired,
    category: string.isRequired,
    subcategory: string,
  }

  render() {
    const
      { category, subcategory, photoId } = this.props,
      props$ = utils.omit(this.props, ['category', 'subcategory', 'photoId']),
      link = subcategory
        ? `${category}/${subcategory}`
        : category;

    return (
      <Link to={`/${link}/photo/${photoId}`} activeClassName="active" {...props$} />
    );
  }
}
