import React from 'react';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import shallowCompare from 'react-addons-shallow-compare';

import CategoryLink from '../link/category';
import PhotoLink from '../link/photo';
import Brick from './brick';
import Loader from '../loader';

import './gallery.less';

const
  { shape, arrayOf, element } = React.PropTypes,
  isBrowser = (typeof window !== 'undefined'),
  Packery = isBrowser ? require('packery') : null;

export default class Gallery extends React.Component {

  static propTypes = {
    children: element,
    category: shape().isRequired,
    photos: arrayOf(shape()).isRequired,
  };

  constructor(props) {
    super(props);

    this.state = {
      isLoading: true,
    };
  }

  componentDidMount() {
    // this.onMount(function callback() {
    this.setState({
      isLoading: false,
    });
    // });

    this.packery = this.createPackery(this.packeryCmp);
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  componentDidUpdate() {
    if (isBrowser) {
      this.packery.doUpdate();
    }
  }

  componentWillUnmount() {
    this.packery.destroy();
    this.packery = null;
  }

  createPackery(container) {
    const packery = new Packery(container, {
      columnWidth: 100,
      itemSelector: 'li',
      gutter: 10,
    });

    packery.doUpdate = () => {
      packery.reloadItems();
      packery.layout();
    };

    return packery;
  }

  render() {
    const
      { isLoading } = this.state,
      { category, photos } = this.props,
      gallery = photos.map(p => (
        <li className="photo" key={p.id} >
          <PhotoLink photoId={p.id} {...CategoryLink.fromCategory(category)}>
            <Brick photo={p} />
          </PhotoLink>
        </li>
      )),
      hasNav = !!(category.parent || category).childs.length,
      childrens = !!photos.length && React.Children.map(this.props.children, c =>
        React.cloneElement(c, {
          photos,
        }));

    return (
      <div className={hasNav ? 'gallery nav' : 'gallery'} >
        <Loader visible={isLoading} />
        <ReactCSSTransitionGroup transitionName="photo" transitionEnterTimeout={200} transitionLeaveTimeout={200}>
          {childrens}
        </ReactCSSTransitionGroup>
        <ul ref={(c) => { this.packeryCmp = c; }} className={isLoading ? 'loading' : ''}>{gallery}</ul>
      </div>
    );
  }
}
