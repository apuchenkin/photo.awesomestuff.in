import React from 'react';
import Link from './index';
import utils from '../../lib/utils';

const { string, number, node } = React.PropTypes;

const PhotoLink = function PhotoLink(props) {
  const
    { category, subcategory, photoId, children } = props,
    props$ = utils.omit(props, ['category', 'subcategory', 'photoId']),
    link = subcategory
      ? `${category}/${subcategory}`
      : category;

  return (
    <Link to={`/${link}/photo/${photoId}`} activeClassName="active" {...props$} >
      {children}
    </Link>
  );
};

PhotoLink.propTypes = {
  children: node.isRequired,
  photoId: number.isRequired,
  category: string.isRequired,
  subcategory: string,
};

export default PhotoLink;
