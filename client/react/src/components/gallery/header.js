import React from 'react';
import CategoryLink from '../link/category';
import Link from 'react-router/lib/Link';

import './navigation.less';

class Header extends React.Component {
	render() {
    let
      props = this.props,
      category = props.category.parent || props.category,
			childrens = props.categories
				.filter(c => c.parent && c.parent.name === category.name)
				.map(c => {
		      return (
						 <li className="item" key={c.id} >
		           <CategoryLink category={c.parent.name} subcategory={c.name}>{c.title}</CategoryLink>
		         </li>
		      )
			});

		return (
			<header className="main" ref="main">
				<h1 className="title">
					<Link to='/' activeClassName="active">HOME</Link> / <CategoryLink category={category.name}>{category.title}</CategoryLink>
				</h1>
        {childrens && <nav className="categories"><ul>{childrens}</ul></nav>}
			</header>
		);
	}
}

Header.propTypes = {
	category: React.PropTypes.object.isRequired,
  categories: React.PropTypes.array.isRequired
};

export default Header
