import React from 'react';
import CategoryLink from '../link/category';
import Link from 'react-router/lib/Link';

import './navigation.less';

class Header extends React.Component {

	constructor(props, context) {
		let
			params = props.params,
			initial = context.initialState;

		super(props, context);

    this.state = {
      params: params,
      categories: initial.categories || []
    }
  }

	componentWillReceiveProps(props) {
		let
			me = this,
			params = props.params;

    this.setState({
			params: params
		});
  }

	render() {
    let
      state = this.state,
      params = state.params,
      category = state.categories.find(c => c.name == params.category),
			categories = state.categories.filter(c => c.parent && c.parent.name === params.category).map(category => {
	      return (
					 <li className="item" key={category.id} >
	           <CategoryLink category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name}>{category.title}</CategoryLink>
	         </li>
	      )
			});

		return (
			<header className="main" ref="main">
				<h1 className="title">
					<Link to='/' activeClassName="active">HOME</Link> / <CategoryLink category={params.category}>{category && category.title}</CategoryLink>
				</h1>
        <nav className="categories"><ul>{categories}</ul></nav>
			</header>
		);
	}
}

Header.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Header
