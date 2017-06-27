import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Link from 'found/lib/Link';

import baseStyle from '../../style/style.less';

const { string, number, node } = React.PropTypes;

const PhotoLink = ({ category, subcategory, photoId, children, ...props }) => {
  const link = subcategory
    ? `${category}/${subcategory}`
    : category;

  return (
    <Link to={`/${link}/photo/${photoId}`} activeClassName={baseStyle.active} {...props} >
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

PhotoLink.defaultProps = {
  subcategory: null,
};

export default withStyles(baseStyle)(PhotoLink);
