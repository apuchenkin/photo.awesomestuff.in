import React from 'react';
import Category from '../link/category';
import Photo from '../link/photo';
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
      categories: initial.categories || props.categories || [],
			photos: initial.photos || []
    }
  }

	// pick(object, params) {
	// 	return Object.keys(object).filter(k => params.indexOf(k) > 0).reduce((o,k) => {o[k] = object[k]; return o}, {})
	// }

	componentDidMount() {
		let me = this;

		if (!me.state.photos.length) {
			Gallery.fetchData(location.origin, me.props.params).photos
				.then(photos => me.setState({photos: photos}));
		}
	}

	componentWillReceiveProps(props) {
		let
			me = this,
			params = props.params;

		if (me.state.category !== params.category
			|| me.state.subcategory !== params.subcategory) {
				this.setState({
					category: params.category,
					subcategory: params.subcategory
				}, () => {
					Gallery.fetchData(location.origin, params).photos
						.then(photos => me.setState({photos: photos}));
				});
			}
  }

	static fetchData (location, params) {
    let
			category = params.subcategory || params.category,
			photoService = new PhotoService(null, location);

    return {
			photos: photoService.fetchPhotos(category).then(p =>
				photoService.remapPhotos(
					photoService.refinePhotos(p, params.photoId)
				)
			)
		}
  }

	render() {
    let
      state = this.state,
      category = state.categories.find(c => c.name == state.category),
      subcategory = state.subcategory && state.categories.find(c => c.name == state.subcategory),
      categories = state.categories.filter(c => c.parent && c.parent.name === state.category).map(category => {
          return (
						 <li className="item" key={category.id} >
	             <Category category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name}>{category.title}</Category>
	           </li>
          );
			}),
			photos = state.photos.map(p => (
				<li className="photo" key={p.id} >
					<Photo photoId={p.id} category={state.category} subcategory={state.subcategory}>({p.id}, {p.group}, {p.views}, [{p.w}x{p.h}])</Photo>
				</li>
			)),
      childrens = state.photos && state.photos.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
        photos: state.photos
      }));

		return (
			<div>
				<h1>Gallery: <Category category={state.category}>{category && category.title}</Category>
					>
					{subcategory && <Category category={state.category} subcategory={state.subcategory} >{subcategory && subcategory.title}</Category>}
				</h1>
        <nav>{categories}</nav>
				<div>{photos}</div>
				<div>{childrens}</div>
			</div>
		);
	}
}

Gallery.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Gallery
