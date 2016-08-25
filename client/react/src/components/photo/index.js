import React from 'react';
import withRouter from 'react-router/lib/withRouter';
import Category from '../link/category';
import PhotoService from '../../service/Photo';
import PhotoLink from '../link/photo';
import resolutions from './resolution.json';
import config from '../../config.json';
import Link from 'react-router/lib/Link';
import './photo.less';

var isBrowser = (typeof window !== 'undefined');

class Photo extends React.Component {

	constructor(props) {
		super(props);

    this.state = {
			category: props.category,
			photos: props.photos,
      photo: props.photo,
			dimensions: {
				width: isBrowser ? window.outerWidth - 40 : props.photo.width / 2,
				height: isBrowser ? window.outerHeight - 40 : props.photo.height / 2
			}
    }
  }

	componentDidMount() {
		props.route.parent.resolve(props.params).then(data =>
			me.setState(data)
		)
	}

	componentWillReceiveProps(props) {
		const
			me = this;

		if (props.params.photoId != me.state.photo.id) {
			  props.route.parent.resolve(props.params).then(data =>
					me.setState(data)
				)
		}
  }

	adjust (w, h) {
		const
			norms = resolutions.map(([w$,h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
			min = Math.min(...norms),
			idx = norms.findIndex(n => n == min)
		;

		return resolutions[idx];
	}

	close() {
		const
			category = this.state.category,
			url = category.parent ? category.parent.name + '/' + category.name : category.name
		;

		this.props.router.push('/' + url);
	}

	goNext(next) {
		const
			category = this.state.category,
			url = category.parent ? category.parent.name + '/' + category.name : category.name
		;

		this.props.router.push('/' + url + '/photo/' + next.id);
	}

	render() {
    const
      state = this.state,
      photo = state.photo,
			category = state.category,
			photos = state.photos,
      pidx = photos.findIndex(p => p.id == photo.id),
      prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1],
      next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1],
			[w, h] = this.adjust (state.dimensions.width, state.dimensions.height),
			filename = photo.src.split('/').pop(),
			src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/'),
			url = '/' + (category.parent ? category.parent.name + '/' + category.name : category.name),
			figure = (
				<figure className="content">
					<div className="tools"><Link onClick={e => e.stopPropagation()} to={url}>CLOSE <i className="icon-cancel"></i></Link></div>
					<img className="photo" onClick={e => {e.stopPropagation(); this.goNext(next)}} src={src} style={{maxHeight: (h - 120) + 'px', maxWidth: w + 'px'}} />
					<figcaption className="description">
						<span className="caption">{photo.caption}</span>
						{photo.author && <div>AUTHOR: <span className="author">{photo.author.name}</span></div>}
					</figcaption>
        </figure>
			)
		;

		return (
			<div className="photo-widget" onClick={this.close.bind(this)}>
				{figure}
				<PhotoLink onClick={e => e.stopPropagation()} category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name} photoId={prev && prev.id} className="nav prev" title="PREV"><i className="icon-left-open" /></PhotoLink>
				<PhotoLink onClick={e => e.stopPropagation()} category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name} photoId={next && next.id} className="nav next" title="NEXT"><i className="icon-right-open" /></PhotoLink>
			</div>
		);
	}
}

Photo.propTypes = {
	category: React.PropTypes.object.isRequired,
  photos: React.PropTypes.array.isRequired,
	photo: React.PropTypes.object.isRequired
}

export default withRouter(Photo);
