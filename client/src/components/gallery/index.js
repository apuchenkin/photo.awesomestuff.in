import React from 'react';
import { connect } from 'react-redux';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Component from '../../lib/PureComponent';
import { stopLoading } from '../../actions/loader';
import Gallery from './gallery';

import style from './gallery.less';
import transitionStyle from '../../style/transition.less';

const
  { shape, arrayOf, element, bool, func } = React.PropTypes;

class GalleryPage extends Component {

  static propTypes = {
    children: element,
    category: shape().isRequired,
    photos: arrayOf(shape()).isRequired,
    stopLoading: func.isRequired,
    isLoading: bool.isRequired,
  };

  componentDidMount() {
    this.props.stopLoading();
  }

  componentWillReceiveProps(props) {
    if (!props.children) {
      props.stopLoading();
    }
  }

  render() {
    const
      { category, photos, isLoading } = this.props,
      hasNav = !!(category.parent || category).childs.length,
      className = [
        style.gallery,
        hasNav ? style.nav : '',
        isLoading ? style.loading : '',
      ].filter(x => !!x).join(' '),
      childrens = !!photos.length && React.Children.map(this.props.children, c =>
        React.cloneElement(c, {
          photos,
        }));

    return (
      <div className={className} >
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
        <Gallery
          photos={photos}
          category={category}
        />
      </div>
    );
  }
}

export default connect(
  state => ({ isLoading: state.isLoading.count > 0 }),
  dispatch => ({
    stopLoading: () => dispatch(stopLoading()),
  })
)(withStyles(style, transitionStyle)(GalleryPage));
