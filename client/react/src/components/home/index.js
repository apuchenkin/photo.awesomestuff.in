import React from 'react';
import CategoryService from '../../service/Category';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import CategoryLink from '../link/category';
import './style.less';

class Home extends React.Component {
	constructor(props, context) {
    let
      params = props.params,
      initial = context.initialState;

    super(props, context);

    this.state = {
			category: params ? params.category : null,
			categories: initial.categories || props.categories || [],
			photos: [],
			groups: [],
			showHidden: false
		}
  }

	render() {
    let
      categories = this.state.categories.filter(c => !!c.title).map(function(category) {
          return (
            <li className="item" key={category.id} >
              <CategoryLink category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name} >{category.title}</CategoryLink>
            </li>
          );
    });

		return (
			<div>
				<h1>Home</h1>
        <nav className="aside">
          <ul>{categories}</ul>
        </nav>
			</div>
		);
	}
}

Home.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Home;
