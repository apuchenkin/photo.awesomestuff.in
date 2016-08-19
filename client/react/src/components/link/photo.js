import React from 'react';
import Link from 'react-router/lib/Link';

export default class Photo extends React.Component {

  propTypes: {
    children: React.PropTypes.element.isRequired,
    category: React.PropTypes.object.isRequired,
    data: React.PropTypes.object.isRequired
  }

  render() {
    let
      props = this.props,
      category = props.category,
			photo = props.data,
			link = category.parent
				? category.parent.name + '/' + category.name
				: category.name

    return (
        <Link to={`/${link}/photo/${photo.id}`} activeClassName="active">{this.props.children}</Link>
      );
  }
}
