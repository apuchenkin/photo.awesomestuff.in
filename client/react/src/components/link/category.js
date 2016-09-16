import React from 'react';
import Link from './index';
import utils from '../../lib/utils';

const { string, node } = React.PropTypes;

function CategoryLink(props) {
  const
    { category, subcategory, children } = props,
    link = subcategory
      ? `${category}/${subcategory}`
      : category;

  return (
    <Link to={`/${link}`} activeClassName="active" {...utils.omit(props, ['category', 'subcategory'])} >
      {children}
    </Link>
  );
}

CategoryLink.propTypes = {
  children: node.isRequired,
  category: string.isRequired,
  subcategory: string,
};

CategoryLink.fromCategory = category => ({
  category: category.parent ? category.parent.name : category.name,
  subcategory: category.parent && category.name,
});

export default CategoryLink;
