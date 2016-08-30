import React from 'react';
import Link from './index';
import utils from '../../lib/utils';

const {any, string, number} = React.PropTypes;

export default class PhotoLink extends React.Component {

  static propTypes = {
    children: any.isRequired,
    category: string.isRequired,
    subcategory: string,
    photoId: number.isRequired
  }

  render() {
    const
      props = this.props,
      {category, subcategory, photoId} = props,
      props$ = utils.omit(props, ['category', 'subcategory', 'photoId']),
      link = subcategory
        ? category + '/' + subcategory
        : category;

    return (
        <Link to={`/${link}/photo/${photoId}`} activeClassName="active" {...props$} />
      );
  }
}
