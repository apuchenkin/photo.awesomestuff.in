import React from 'react';
import Category from '../link/category';
import PhotoService from '../../service/Photo';
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
      photo = state.photo

		return (
			<div>
				<h1>Photo: {photo.id}</h1>
			</div>
		);
	}
}

Photo.contextTypes = {
  initialState: React.PropTypes.any.isRequired
}

export default Photo;
