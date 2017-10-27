import React from 'react';
import { shape, element, arrayOf, bool } from 'prop-types';
import { connect } from 'react-redux';
import { CSSTransitionGroup } from 'react-transition-group';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Gallery from '../components/gallery/gallery';

import style from '../components/gallery/gallery.less';
import transitionStyle from '../style/transition.less';

const GalleryPage = ({ category, categories, loading, children }) => {
  const hasNav = categories.some(c => c.parent
    && c.parent.id === (category.parent || category).id,
  );
  const className = [
    style.gallery,
    hasNav ? style.nav : '',
    loading ? style.loading : '',
  ].filter(x => !!x).join(' ');

  return (
    <div className={className} >
      {/* <CSSTransitionGroup
        transitionName={{
          enter: transitionStyle['fade-enter'],
          enterActive: transitionStyle['fade-enter-active'],
          leave: transitionStyle['fade-leave'],
          leaveActive: transitionStyle['fade-leave-active'],
        }}
        transitionEnterTimeout={200}
        transitionLeaveTimeout={200}
      > */}
        {children}
      {/* </CSSTransitionGroup> */}
      <Gallery category={category} />
    </div>
  );
};

GalleryPage.propTypes = {
  children: element,
  category: shape().isRequired,
  categories: arrayOf(shape()).isRequired,
  loading: bool.isRequired,
};

GalleryPage.defaultProps = {
  children: null,
};

export default connect(
  ({ category: { category, categories, loading } }) => ({
    category,
    categories,
    loading,
  }),
)(withStyles(style, transitionStyle)(GalleryPage));
