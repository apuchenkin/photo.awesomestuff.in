import React from 'react';
import CategoryService from '../../../../admin/src/service/Category';
import './style.less';

export default class Home extends React.Component {

	constructor(props) {
	    super(props);
	    this.state ={
				category: props.params ? props.params.category : null,
				categories: [props.params.locale],
				photos: [],
				groups: [],
				showHidden: false
			}

	    console.log(props);
	  }


	fetchCategories () {
    let me = this,
        categoryService = new CategoryService(me.state.token);

    categoryService.fetchCategories()
      .then(categories => me.setState({categories: categories}));
  }

	componentDidMount() {
		this.fetchCategories();
	}

	render() {
		return (
			<div>
				<h1>Home</h1>
				<p>{this.state.categories}</p>
			</div>
		);
	}
}
