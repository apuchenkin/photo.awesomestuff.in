import React from 'react';
import Link from 'react-router/lib/Link';

class PhotoLink extends React.Component {
  render() {
    let
      props = this.props,
			link = props.subcategory
				? props.category + '/' + props.subcategory
				: props.category

    return (
        <Link to={`/${link}/photo/${props.photoId}`} activeClassName="active">{props.children}</Link>
      );
  }
}

PhotoLink.propTypes = {
  children: React.PropTypes.any.isRequired,
  category: React.PropTypes.string.isRequired,
  subcategory: React.PropTypes.string,
  photoId: React.PropTypes.number
}

export default PhotoLink;
