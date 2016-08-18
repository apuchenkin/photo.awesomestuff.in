import React from 'react';
import Link from 'react-router/lib/Link';

export default class Category extends React.Component {
  render() {
    let
			category = this.props.data,
			link = category.parent
				? category.parent.name + '/' + category.name
				: category.name

    return (
        <Link to={`/${link}`} activeClassName="active">{category.title}</Link>
      );
  }
}
