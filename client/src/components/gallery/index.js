import React from 'react';
import { shape, element, bool } from 'prop-types';
import { connect } from 'react-redux';
import { CSSTransitionGroup } from 'react-transition-group';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

// import { startLoading, stopLoading } from '../../actions/loader';
import Gallery from './gallery';

import style from './gallery.less';
import transitionStyle from '../../style/transition.less';


class GalleryPage extends React.PureComponent {
  componentWillMount() {
    // if (isBrowser) {
    //   // stops the loading initiated by server
    //   this.props.stopLoading();
    // }
  }

  render() {
    const { category, isLoading, children } = this.props;
    // const hasNav = !!(category.parent || category).childs.length;
    const className = [
      style.gallery,
      // hasNav ? style.nav : '',
      isLoading ? style.loading : '',
    ].filter(x => !!x).join(' ');

    return (
      <div className={className} >
        <CSSTransitionGroup
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
        </CSSTransitionGroup>
        <Gallery category={category} />
      </div>
    );
  }
}

GalleryPage.propTypes = {
  children: element,
  category: shape().isRequired,
  // stopLoading: func.isRequired,
  isLoading: bool.isRequired,
};

GalleryPage.defaultProps = {
  children: null,
};

export default connect(
  ({ category: { category } }) => ({
    isLoading: false,
    category,
  }),
  // dispatch => ({
  //   startLoading: () => dispatch(startLoading()),
  //   stopLoading: () => dispatch(stopLoading()),
  // })
)(withStyles(style, transitionStyle)(GalleryPage));
