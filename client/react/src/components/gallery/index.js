import React from 'react';
import { connect } from 'react-redux';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import { startLoading, stopLoading } from '../../actions/loader';
import { fromCategory } from '../link/category';
import PhotoLink from '../link/photo';
import Brick from './brick';

import style from './gallery.less';
import transitionStyle from '../../style/transition.less';

const
  { shape, arrayOf, element, bool, func } = React.PropTypes,
  isBrowser = (typeof window !== 'undefined'),
  Packery = isBrowser ? require('packery') : null;

class Gallery extends React.Component {

  static propTypes = {
    children: element,
    category: shape().isRequired,
    photos: arrayOf(shape()).isRequired,
    startLoading: func.isRequired,
    stopLoading: func.isRequired,
    isLoading: bool.isRequired,
  };

  constructor(props) {
    super(props);
    props.startLoading();
  }

  componentDidMount() {
    this.packery = this.createPackery(this.packeryCmp);
    this.props.stopLoading();
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
      { category, photos, isLoading } = this.props,
      gallery = photos.map(p => (
        <li key={p.id} >
          <PhotoLink photoId={p.id} {...fromCategory(category)}>
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
      <div className={hasNav ? `${style.gallery} ${style.nav}` : `${style.gallery}`} >
        <ReactCSSTransitionGroup
          transitionName={{
            enter: transitionStyle['fade-enter'],
            enterActive: transitionStyle['fade-enter-active'],
            leave: transitionStyle['fade-leave'],
            leaveActive: transitionStyle['fade-leave-active'],
          }}
          transitionEnterTimeout={200}
          transitionLeaveTimeout={200}
        >
          {childrens}
        </ReactCSSTransitionGroup>
        <ul ref={(c) => { this.packeryCmp = c; }} className={isLoading ? style.loading : ''}>{gallery}</ul>
      </div>
    );
  }
}

export default connect(
  state => ({ isLoading: state.isLoading.count > 0 }),
  dispatch => ({
    startLoading: () => dispatch(startLoading()),
    stopLoading: () => dispatch(stopLoading()),
  })
)(withStyles(style, transitionStyle)(Gallery));
