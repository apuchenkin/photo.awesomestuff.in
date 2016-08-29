import React from 'react';
import Link from 'react-router/lib/Link';
import utils from '../../lib/utils';

export default class CategoryLink extends React.Component {
  render() {
    let
      props = this.props,
      props$ = utils.pick(props, ['className', 'title', 'style', 'onClick']),
			link = props.subcategory
				? props.category + '/' + props.subcategory
				: props.category

    return (
        <Link to={`/${link}`} activeClassName="active" {...props$}>{this.props.children}</Link>
      );
  }
}

CategoryLink.propTypes = {
  children: React.PropTypes.any.isRequired,
  category: React.PropTypes.string.isRequired,
  subcategory: React.PropTypes.string
}
