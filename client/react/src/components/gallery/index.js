import React from 'react';
import CategoryLink from '../link/category';
import PhotoLink from '../link/photo';
import PhotoService from '../../service/Photo';
import config from '../../config.json';
import Link from 'react-router/lib/Link';
import Brick from './brick';
import './gallery.less';

var isBrowser = (typeof window !== 'undefined');
var Packery = isBrowser ? window.Packery || require('packery') : null;

class Gallery extends React.Component {
	constructor(props, context) {
		let
			params = props.params,
			initial = context.initialState;

		super(props, context);

    this.state = {
      params: params,
			photos: initial.photos || []
    }
  }

	componentDidMount() {
		const
			me = this,
			props = me.props
		;

		me.packery = me.createPackery(me.refs.packery);

		if (!me.state.photos.length) {
			me.props.route.resolve(props.params).photos
				.then(photos => me.setState({photos: photos}));
		}
	}

	componentDidUpdate() {
		console.log('componentDidUpdate');
		if (!isBrowser) return;

		this.packery.doUpdate();
	}

	createPackery(container) {
	  var packery = new Packery(container, {
	    columnWidth: 100,
	    itemSelector: 'li',
	    gutter: 10
	  });

	  packery.defer = [];

	  packery.on('layoutComplete', function() {
	    packery.isLoading = false;
	    if (packery.defer.length) {
	      packery.defer.pop().apply(packery);
	    }
	  });

	  packery.doUpdate = function() {
	    packery.reloadItems();
	    packery.layout();

	    if (!packery.isLoading) {
	      packery.isLoading = true;
	    } else {
	      packery.defer.push(packery.doUpdate);
	    }
	  }

	  return packery;
	}

	componentWillReceiveProps(props) {
		let
			me = this,
			params = props.params;

		console.log('componentWillReceiveProps');
		// this.packery.doUpdate();
		if (me.state.params !== params) {
				this.setState({
					params: params
				}, () => {
					props.route.resolve(params).photos
						.then(photos => me.setState({photos: photos}));
				});
			}
  }

	render() {
    let
      state = this.state,
			params = state.params,
			photos = state.photos.map(p => (
				<li className="photo" key={p.id} >
					<PhotoLink photoId={p.id} category={params.category} subcategory={params.subcategory}>
						<Brick photo={p} />
					</PhotoLink>
				</li>
			)),
      childrens = state.photos && !!state.photos.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
        photos: state.photos
      }));

		return (
				<div className="gallery"><ul ref="packery">{photos}</ul>{childrens}</div>
		);
	}
}

Gallery.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

export default Gallery
