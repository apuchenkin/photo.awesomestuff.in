import React from 'react';
import Link from 'react-router/lib/Link';

export default class CategoryLink extends React.Component {
  render() {
    let
      props = this.props,
			link = props.subcategory
				? props.category + '/' + props.subcategory
				: props.category

    return (
        <Link to={`/${link}`} activeClassName="active">{this.props.children}</Link>
      );
  }
}

CategoryLink.propTypes = {
  children: React.PropTypes.any.isRequired,
  category: React.PropTypes.string.isRequired,
  subcategory: React.PropTypes.string
}

export default CategoryLink;
