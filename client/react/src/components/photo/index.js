import React from 'react';
import withRouter from 'react-router/lib/withRouter';
import Category from '../link/category';
import PhotoService from '../../service/Photo';
import PhotoLink from '../link/photo';
import resolutions from './resolution.json';
import config from '../../config.json';
import Link from 'react-router/lib/Link';
import Loader from '../loader';
import './photo.less';
import utils from '../../lib/utils';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import {injectIntl, intlShape, FormattedMessage} from 'react-intl';
import { bind, memoize, debounce } from 'decko';

var isBrowser = (typeof window !== 'undefined');

class Photo extends React.Component {

	constructor(props) {
		super(props);

    this.state = {
			isLoading: true,
			dimensions: {
				width: isBrowser ? window.innerWidth - 40 : props.photo.width / 2,
				height: isBrowser ? window.innerWidth - 40 : props.photo.height / 2
			}
    }
  }

	componentDidMount() {
		window.addEventListener('resize', this.resize);
	}

	componentWillUnmount() {
		window.removeEventListener('resize', this.resize);
	}


	@bind
	@debounce(50)
	resize() {
		this.setState({
			dimensions: {
				width: isBrowser ? window.innerWidth - 40 : props.photo.width / 2,
				height: isBrowser ? window.innerWidth - 40 : props.photo.height / 2
			}
		});
	}

	@bind
	adjust (w, h) {
		const
			norms = resolutions.map(([w$,h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
			min = Math.min(...norms),
			idx = norms.findIndex(n => n == min)
		;

		return resolutions[idx];
	}

	@bind
	close() {
		const
			category = this.props.category,
			url = category.parent ? category.parent.name + '/' + category.name : category.name
		;

		this.props.router.push('/' + url);
	}

	@bind
	goNext(next) {
		const
			category = this.props.category,
			url = category.parent ? category.parent.name + '/' + category.name : category.name
		;

		this.props.router.push('/' + url + '/photo/' + next.id);
	}

	@bind
	onLoad() {
		this.setState({isLoading: false});
	}

	render() {
    const
      state = this.state,
			props = this.props,
			intl = props.intl,
      photo = props.photo,
			category = props.category,
			photos = props.photos,
      pidx = photos.findIndex(p => p.id == photo.id),
      prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1],
      next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1],
			[w, h] = this.adjust (state.dimensions.width, state.dimensions.height),
			filename = photo.src.split('/').pop(),
			src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/'),
			url = '/' + (category.parent ? category.parent.name + '/' + category.name : category.name),
			closeIcon = <FormattedMessage
				id="icon.close"
				defaultMessage={`Close {icon}`}
				values={{icon: (<i className="icon-cancel"></i>)}}
				/>,
			figure = (
				<figure className={this.state.isLoading ? "content loading" : "content"} >
					<div className="tools"><Link onClick={e => e.stopPropagation()} to={url}>{closeIcon}</Link></div>
					<img className="photo" onClick={e => {e.stopPropagation(); this.goNext(next)}} src={src} style={{maxHeight: (h - 120) + 'px', maxWidth: w + 'px'}} onLoad={this.onLoad} />
					<figcaption className="description">
						<span className="caption">{photo.caption}</span>
						{photo.author && <div><FormattedMessage
							id="photo.author"
							defaultMessage={`Author: {author}`}
							values={{author: (<span className="author">{photo.author.name}</span>)}}
							/></div>}
					</figcaption>
        </figure>
			)
		;

		return (
			<div className="photo-widget" onClick={this.close}>
				<ReactCSSTransitionGroup transitionName="loader" transitionAppearTimeout={200} transitionEnterTimeout={200} transitionLeaveTimeout={200} transitionAppear={false}>
					{this.state.isLoading && <Loader />}
				</ReactCSSTransitionGroup>

				{figure}
				<PhotoLink
					onClick={e => e.stopPropagation()}
					category={category.parent ? category.parent.name : category.name}
					subcategory={category.parent && category.name}
					photoId={prev && prev.id}
					className="nav prev"
					title={intl.formatMessage({id: 'prev'})}><i className="icon-left-open" />
				</PhotoLink>
				<PhotoLink onClick={e => e.stopPropagation()}
					category={category.parent ? category.parent.name : category.name}
					subcategory={category.parent && category.name}
					photoId={next && next.id}
					className="nav next"
					title={intl.formatMessage({id: 'next'})}><i className="icon-right-open" />
				</PhotoLink>
			</div>

		);
	}
}

Photo.propTypes = {
	category: React.PropTypes.object.isRequired,
  photos: React.PropTypes.array.isRequired,
	photo: React.PropTypes.object.isRequired,
	intl: intlShape.isRequired,
	router: React.PropTypes.object.isRequired
}

export default withRouter(injectIntl(Photo));
