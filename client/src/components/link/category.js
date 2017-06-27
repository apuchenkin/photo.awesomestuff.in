import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Link from 'found/lib/Link';

import baseStyle from '../../style/style.less';

const { string, node } = React.PropTypes;

function CategoryLink({ category, subcategory, children, ...props }) {
  const link = subcategory
    ? `${category}/${subcategory}`
    : category;

  return (
    <Link to={`/${link}`} activeClassName={baseStyle.active} {...props} >
      {children}
    </Link>
  );
}

CategoryLink.propTypes = {
  children: node.isRequired,
  category: string.isRequired,
  subcategory: string,
};

CategoryLink.defaultProps = {
  subcategory: null,
};

export const fromCategory = category => ({
  category: category.parent ? category.parent.name : category.name,
  subcategory: category.parent && category.name,
});

export default withStyles(baseStyle)(CategoryLink);
