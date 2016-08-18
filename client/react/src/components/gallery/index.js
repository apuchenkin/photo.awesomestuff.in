import React from 'react';
import Category from '../link/category';
import PhotoService from '../../service/Photo';
// import './style.less';

class Gallery extends React.Component {

	constructor(props, context) {
		let
			params = props.params,
			initial = context.initialState;

		super(props, context);

    this.state = {
      category: params.category,
      subcategory: params.subcategory,
      categories: initial.categories || [],
			photos: initial.photos || []
    }
  }

	componentDidMount() {
		let me = this;
		Gallery.fetchData(location.origin).photos
			.then(photos => me.setState({photos: photos}));
	}

	componentWillReceiveProps(props) {
		let params = props.params;

    this.setState({
			category: params.category,
			subcategory: params.subcategory
		}, () => {
			console.log(123);
		});
  }

	static fetchData (location) {
    let
			state = this.state,
			categoryName = state.subcategory || state.category,
			category = state.categories.find(c => c.name == categoryName)
			photoService = new PhotoService(null, location);

    return {
			photos: photoService.fetchPhotos(category.id)
		}
  }

	render() {
    let
      state = this.state,
      category = state.categories.find(c => c.name == state.category),
      subcategory = state.subcategory && state.categories.find(c => c.name == state.subcategory),
      childs = state.categories.filter(c => c.parent && c.parent.name === state.category).map(category => {
          return (
						 <li className="item" key={category.id} >
	             <Category data={category} />
	           </li>
          );
    });

		return (
			<div>
				<h1>Gallery: <Category data={category} />/{subcategory && <Category data={subcategory} />}</h1>
        <nav>{childs}</nav>
				<div>{state.photos.map(p => p.id)}</div>
			</div>
		);
	}
}

Gallery.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Gallery
