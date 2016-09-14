import React from 'react';
import Link from './index';
import utils from '../../lib/utils';

const { string } = React.PropTypes;

export default class CategoryLink extends React.Component {

  static fromCategory(category) {
    return {
      category: category.parent ? category.parent.name : category.name,
      subcategory: category.parent && category.name,
    };
  }

  static propTypes = {
    category: string.isRequired,
    subcategory: string,
  }

  render() {
    const
      { category, subcategory } = this.props,
      link = subcategory
        ? `${category}/${subcategory}`
        : category;

    return (
      <Link to={`/${link}`} activeClassName="active" {...utils.omit(this.props, ['category', 'subcategory'])} />
    );
  }
}
