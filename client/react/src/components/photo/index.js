import React from 'react';
import Category from '../link/category';
import PhotoService from '../../service/Photo';
import PhotoLink from '../link/photo';
// import './style.less';

class Photo extends React.Component {

	constructor(props, context) {
		let
			params = props.params,
			initial = context.initialState;

		super(props, context);

    this.state = {
      params: params,
			photos: initial.photos || [],
      photo: initial.photo || []
    }
  }

	componentDidMount() {
		let me = this;

		Photo.fetchData(location.origin, me.props.params).photo
			.then(photo => me.setState({photo: photo}));
	}

	componentWillReceiveProps(props) {
		let
			me = this,
			params = props.params;

    this.setState({
			params: params
		}, () => {
      Photo.fetchData(location.origin, me.props.params).photo
  			.then(photo => me.setState({photo: photo}));
		});
  }

	static fetchData (location, params) {
    let
			photoService = new PhotoService(null, location);

    return {
			photo: photoService.fetchPhoto(params.photoId)
		}
  }

	render() {
    let
      state = this.state,
      photo = state.photo,
      category = state.params.category,
      subcategory = state.params.subcategory,
      pidx = state.photos.findIndex(p => p.id == state.params.photoId),
      prev = state.photos[pidx - 1 < 0 ? state.photos.length - 1 : pidx - 1],
      next = state.photos[pidx + 1 > state.photos.length - 1 ? 0 : pidx + 1]
    ;

		return (
			<div>
				<h1>Photo:
          <PhotoLink category={category} subcategory={subcategory} photoId={prev.id}>prev</PhotoLink>
          {photo.id}
          <PhotoLink category={category} subcategory={subcategory} photoId={next.id}>next</PhotoLink>
        </h1>
			</div>
		);
	}
}

Photo.contextTypes = {
  initialState: React.PropTypes.any.isRequired
}

export default Photo;
