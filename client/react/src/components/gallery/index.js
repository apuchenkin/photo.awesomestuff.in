import React from 'react';
import CategoryLink from '../link/category';
import PhotoLink from '../link/photo';
import PhotoService from '../../service/Photo';
import config from '../../config.json';
import Link from 'react-router/lib/Link';
import Brick from './brick';
import Loader from '../loader';
import './gallery.less';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';

var isBrowser = (typeof window !== 'undefined');
var Packery = isBrowser ? window.Packery || require('packery') : null;

class Gallery extends React.Component {
	constructor(props) {
		super(props);

    this.state = {
			isLoading: true
    }
  }

	componentDidMount() {
		const
			me = this
		;

		// debugger;

		me.setState({isLoading: false});
		me.props.route.cmp = me;
		me.packery = me.createPackery(me.refs.packery);
	}

	componentWillUnmount() {
		const
			me = this
		;

		me.packery.destroy();
		me.packery = null;
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

	render() {
    let
      props = this.props,
			state = this.state,
			category = props.category,
			photos = props.photos.map(p => (
				<li className="photo" key={p.id} >
					<PhotoLink photoId={p.id} category={category.parent ? category.parent.name : category.name} subcategory={category.parent && category.name}>
						<Brick photo={p} />
					</PhotoLink>
				</li>
			)),
			hasNav = !!(category.parent || category).childs.length,
      childrens = props.photos && !!props.photos.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
        photos: props.photos
      }));

		return (
				<div className={hasNav ? 'gallery nav' : 'gallery'} >
					<ReactCSSTransitionGroup transitionName="loader" transitionEnterTimeout={200} transitionLeaveTimeout={200}>
            {state.isLoading && <Loader />}
          </ReactCSSTransitionGroup>
					<ReactCSSTransitionGroup transitionName="photo" transitionEnterTimeout={200} transitionLeaveTimeout={200}>
						{childrens}
					</ReactCSSTransitionGroup>
					<ul ref="packery" className={state.isLoading ? 'loading' : ''}>{photos}</ul>
				</div>
		);
	}
}

Gallery.propTypes = {
  category: React.PropTypes.object.isRequired,
	photos: React.PropTypes.array.isRequired
};

export default Gallery
