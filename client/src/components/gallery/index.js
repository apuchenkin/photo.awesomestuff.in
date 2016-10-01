import React from 'react';
import { connect } from 'react-redux';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Component from '../../lib/PureComponent';
import { startLoading, stopLoading } from '../../actions/loader';
import Gallery from './gallery';

import style from './gallery.less';
import transitionStyle from '../../style/transition.less';

const
  { shape, element, bool, func } = React.PropTypes;

class GalleryPage extends Component {

  static propTypes = {
    children: element,
    category: shape().isRequired,
    stopLoading: func.isRequired,
    isLoading: bool.isRequired,
  };

  componentWillMount() {
    if (isBrowser) {
      // stops the loading initiated by server
      this.props.stopLoading();
    }
  }

  render() {
    const
      { category, isLoading, children } = this.props,
      hasNav = !!(category.parent || category).childs.length,
      className = [
        style.gallery,
        hasNav ? style.nav : '',
        isLoading ? style.loading : '',
      ].filter(x => !!x).join(' ');

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
          {children}
        </ReactCSSTransitionGroup>
        <Gallery
          category={category}
        />
      </div>
    );
  }
}

export default connect(
  state => ({
    isLoading: state.isLoading.count > 0,
    category: state.api.category,
  }),
  dispatch => ({
    startLoading: () => dispatch(startLoading()),
    stopLoading: () => dispatch(stopLoading()),
  })
)(withStyles(style, transitionStyle)(GalleryPage));
