import React from 'react';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import shallowCompare from 'react-addons-shallow-compare';

import CategoryLink from '../link/category';
import PhotoLink from '../link/photo';
import Brick from './brick';
import Loader from '../loader';

import './gallery.less';

const
  { object, array } = React.PropTypes,
  isBrowser = (typeof window !== 'undefined'),
  Packery = isBrowser ? window.Packery || require('packery') : null;

export default class Gallery extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: true
    };
  }

  static propTypes = {
    category: object.isRequired,
    photos: array.isRequired
  };

  componentDidMount() {
    const
      me = this
    ;

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
    if (isBrowser) {
      this.packery.doUpdate();
    }
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  createPackery(container) {
    let packery = new Packery(container, {
      columnWidth: 100,
      itemSelector: 'li',
      gutter: 10
    });

    packery.doUpdate = function() {
      packery.reloadItems();
      packery.layout();
    };

    return packery;
  }

  render() {
    const
      props = this.props,
      state = this.state,
      category = props.category,
      photos = props.photos.map(p => (
        <li className="photo" key={p.id} >
          <PhotoLink photoId={p.id} {...CategoryLink.fromCategory(category)}>
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
