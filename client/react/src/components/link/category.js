import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from './index';
import utils from '../../lib/utils';

import baseStyle from '../../style/style.less';

const { string, node } = React.PropTypes;

function CategoryLink(props) {
  const
    { category, subcategory, children } = props,
    link = subcategory
      ? `${category}/${subcategory}`
      : category;

  return (
    <Link to={`/${link}`} activeClassName={baseStyle.active} {...utils.omit(props, ['category', 'subcategory'])} >
      {children}
    </Link>
  );
}

CategoryLink.propTypes = {
  children: node.isRequired,
  category: string.isRequired,
  subcategory: string,
};

export const fromCategory = category => ({
  category: category.parent ? category.parent.name : category.name,
  subcategory: category.parent && category.name,
});

export default withStyles(baseStyle)(CategoryLink);
