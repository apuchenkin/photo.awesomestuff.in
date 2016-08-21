import React from 'react';
import Category from '../link/category';
import PhotoService from '../../service/Photo';
import PhotoLink from '../link/photo';
import resolutions from './resolution.json';
import config from '../../config.json';
import './photo.less';

class Photo extends React.Component {

	constructor(props, context) {
		let
			params = props.params,
			initial = context.initialState;

		super(props, context);

    this.state = {
      params: params,
			photos: initial.photos || props.photos || [],
      photo: initial.photo
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

	adjust (w, h) {
		let
			norms = resolutions.map(([w$,h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
			min = Math.min(...norms),
			idx = norms.findIndex(n => n == min)
		;

		return resolutions[idx];
	}

	render() {
    const
      state = this.state,
      photo = state.photo,
      category = state.params.category,
      subcategory = state.params.subcategory,
      pidx = state.photos.findIndex(p => p.id == state.params.photoId),
      prev = state.photos[pidx - 1 < 0 ? state.photos.length - 1 : pidx - 1],
      next = state.photos[pidx + 1 > state.photos.length - 1 ? 0 : pidx + 1]
    ;

		let figure;

		if (photo) {
			const
				[w, h] = this.adjust (photo.width - 40, photo.height - 40),
				filename = photo.src.split('/').pop(),
				src = [config.apiEndpoint, 'hs/photo', photo.id, w, h, filename].join('/')
			;

			figure = (
				<figure className="content">
					<img className="photo" src={src} style={{maxHeight: (h - 120) + 'px'}} />
					<figcaption className="description">
						<span className="caption">{photo.caption}</span>
						(photo.author && <div>AUTHOR: <span className="author">{photo.author.name}</span></div>)
					</figcaption>
        </figure>
			)
		}

		return (
			<div className="photo-widget">
				<div className="tools">close</div>
				{figure}
				<PhotoLink category={category} subcategory={subcategory} photoId={prev && prev.id} className="nav prev" title="PREV"><i className="icon-left-open" /></PhotoLink>
				<PhotoLink category={category} subcategory={subcategory} photoId={next && next.id} className="nav next" title="NEXT"><i className="icon-right-open" /></PhotoLink>
			</div>
		);
	}
}

Photo.contextTypes = {
  initialState: React.PropTypes.any.isRequired
}

export default Photo;
