import React from 'react';
import CategoryService from '../../service/Category';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import Category from '../link/category';
import './style.less';

class Categories extends React.Component {
  render() {
    let
      categories = this.props.data.map(function(category) {
          return (
            <li className="item" key={category.id} >
              <Category data={category} />
            </li>
          );
    });

    return (
      <nav className="aside">
        <ul>{categories}</ul>
      </nav>
    );
  }
}

class Home extends React.Component {
	constructor(props, context) {
	    super(props, context);

	    this.state = {
				category: props.params ? props.params.category : null,
				categories: context.initialState ? context.initialState.categories || [] : [],
				photos: [],
				groups: [],
				showHidden: false
			}
	  }

	render() {
		return (
			<div>
				<h1>Home</h1>
				<Categories data={this.state.categories} />
			</div>
		);
	}
}

Home.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Home;
