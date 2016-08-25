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
	constructor(props) {
		super(props);

    this.state = {
      category: props.category,
			photos: props.photos || []
    }
  }

	componentDidMount() {
		const
			me = this,
			props = me.props
		;

		me.packery = me.createPackery(me.refs.packery);
		props.route.parent.resolve(props.params).then(data => {
			this.setState(Object.assign(data, {
				category: props.category,
			}))
		})
	}

	componentDidUpdate() {
		console.log('componentDidUpdate');

		if (isBrowser) {
			this.packery.doUpdate();
		}
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
			state = this.state;

		if (state.category.id != props.category.id) {
			props.route.parent.resolve(props.params).then(data => {
				this.setState(Object.assign(data, {
					category: props.category,
				}))
			})
		}
  }

	render() {
    let
      state = this.state,
			category = state.category,
			photos = state.photos.map(p => (
				<li className="photo" key={p.id} >
					<PhotoLink photoId={p.id} category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name}>
						<Brick photo={p} />
					</PhotoLink>
				</li>
			)),
			hasNav = !!(category.parent || category).childs.length
      // childrens = state.photos && !!state.photos.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
      //   photos: state.photos
      // }));

		return (
				<div className={hasNav ? 'gallery nav' : 'gallery'} >
					{this.props.children}
					<ul ref="packery">{photos}</ul>
				</div>
		);
	}
}

Gallery.propTypes = {
  category: React.PropTypes.object.isRequired,
	photos: React.PropTypes.array.isRequired
};

export default Gallery
