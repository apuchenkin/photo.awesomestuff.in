import React from 'react';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import CategoryLink from '../link/category';
import PhotoLink from '../link/photo';
import Brick from './brick';
import Loader from '../loader/loader';

import style from './gallery.less';

const
  { shape, arrayOf, element } = React.PropTypes,
  isBrowser = (typeof window !== 'undefined'),
  Packery = isBrowser ? require('packery') : null;

class Gallery extends React.Component {

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
    this.packery = this.createPackery(this.packeryCmp, () => {
      this.setState({
        isLoading: false,
      });
    });
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

  createPackery(container, callback) {
    const packery = new Packery(container, {
      columnWidth: 100,
      itemSelector: 'li',
      gutter: 10,
    });

    packery.doUpdate = () => {
      packery.reloadItems();
      packery.layout();
    };

    callback();
    return packery;
  }

  render() {
    const
      { isLoading } = this.state,
      { category, photos } = this.props,
      gallery = photos.map(p => (
        <li key={p.id} >
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
      <div className={hasNav ? `${style.gallery} ${style.nav}` : `${style.gallery}`} >
        <Loader visible={isLoading} />
        <ReactCSSTransitionGroup transitionName="photo" transitionEnterTimeout={200} transitionLeaveTimeout={200}>
          {childrens}
        </ReactCSSTransitionGroup>
        <ul ref={(c) => { this.packeryCmp = c; }} className={isLoading ? style.loading : ''}>{gallery}</ul>
      </div>
    );
  }
}

export default withStyles(style)(Gallery);
