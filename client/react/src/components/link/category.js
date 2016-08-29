import React from 'react';
import Link from './index';
import utils from '../../lib/utils';

const {any, string} = React.PropTypes;

export default class CategoryLink extends React.Component {

  static fromCategory(category) {
    return {
      category: category.parent ? category.parent.name : category.name,
      subcategory: category.parent && category.name
    }
  }

  static propTypes = {
    children: any.isRequired,
    category: string.isRequired,
    subcategory: string
  }

  render() {
    let
      props = this.props,
      {category, subcategory} = props,
			link = subcategory
				? category + '/' + subcategory
				: category

    return (
        <Link to={`/${link}`} activeClassName="active" {...utils.omit(props, ['category', 'subcategory'])} />
      );
  }
}
