import { h, Component } from 'preact';
import CategoryService from '../../../../admin/src/service/Category';
import './style.less';

export default class Home extends Component {
	getInitialState() {
		return {
			category: this.props.params ? this.props.params.category : null,
			categories: [],
			photos: [],
			groups: [],
			showHidden: false
		};
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
