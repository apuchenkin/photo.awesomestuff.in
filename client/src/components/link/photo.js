import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from './index';
import utils from '../../lib/utils';

import baseStyle from '../../style/style.less';

const { string, number, node } = React.PropTypes;

const PhotoLink = function PhotoLink(props) {
  const
    { category, subcategory, photoId, children } = props,
    props$ = utils.omit(props, ['category', 'subcategory', 'photoId']),
    link = subcategory
      ? `${category}/${subcategory}`
      : category;

  return (
    <Link to={`/${link}/photo/${photoId}`} activeClassName={baseStyle.active} {...props$} >
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

export default withStyles(baseStyle)(PhotoLink);
