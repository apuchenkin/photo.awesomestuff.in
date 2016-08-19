import React from 'react';
import CategoryLink from '../link/category';
import PhotoLink from '../link/photo';
import PhotoService from '../../service/Photo';
import config from '../../config.json';
import Link from 'react-router/lib/Link';
import './gallery.less';

class Brick extends React.Component {
	constructor (props, context) {
		super(props, context);

		this.state = {
	      photo: props.photo
	    }
		}

		render() {
	    let
				photo = this.state.photo,
				{ w, h, ratio } = photo,
	      inc = ratio >= 1 ? ratio : 1 / ratio,
				[m1,m2] = w < h ? [Math.ceil(w * inc), h] : [Math.ceil(h * inc), w],
				s = Math.max(m1, m2),
				filename = photo.src.split('/').pop(),
				src = [config.apiEndpoint, 'hs/photo', photo.id, s, s, filename].join('/')
			;

			return (
				<div className="brick" style={{width: w + 'px', height: h + 'px', backgroundImage: `url(${src})`}} />
			);
		}
}

Brick.propTypes = {
  photo: React.PropTypes.object.isRequired
}

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
	             <CategoryLink category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name}>{category.title}</CategoryLink>
	           </li>
          );
			}),
			photos = state.photos.map(p => (
				<li className="photo" key={p.id} >
					<PhotoLink photoId={p.id} category={state.category} subcategory={state.subcategory}>
						<Brick photo={p} />
					</PhotoLink>
				</li>
			)),
      childrens = state.photos && state.photos.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
        photos: state.photos
      }));

		return (
			<div className="content">
				<h1><Link to='/' activeClassName="active">HOME</Link>> <CategoryLink category={state.category}>{category && category.title}</CategoryLink>
				</h1>
        <nav>{categories}</nav>
				<div className="gallery">{photos}</div>
				<div>{childrens}</div>
			</div>
		);
	}
}

Gallery.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Gallery
