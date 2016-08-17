import React from 'react';
import CategoryService from '../../../../admin/src/service/Category';
import './style.less';

class Home extends React.Component {
	constructor(props, context) {
	    super(props, context);

	    this.state = {
				category: props.params ? props.params.category : null,
				categories: context.initialState ? context.initialState.categories : [],
				photos: [],
				groups: [],
				showHidden: false
			}
	  }


	static fetchData (location) {
    let categoryService = new CategoryService(null, location);

    return {
			categories: categoryService.fetchCategories()
		}
  }

	componentDidMount() {
		this.fetchData(location.href)
			.then(categories => me.setState({categories: categories}));
	}

	render() {
		return (
			<div>
				<h1>Home1</h1>
				<p>{this.state.categories.map(c => c.name)}</p>
			</div>
		);
	}
}

Home.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Home;
