import React from 'react';
import { shape, element, arrayOf, bool } from 'prop-types';
import classnames from 'classnames';
// import { connect } from 'react-redux';
// import { CSSTransitionGroup } from 'react-transition-group';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { Helmet } from 'react-helmet';

import Main from './main';
import Header from '../components/gallery/header';
import Gallery from '../components/gallery/gallery';

import style from '../components/gallery/gallery.less';
import transitionStyle from '../style/transition.less';

const GalleryPage = ({
  data: {
    category,
    categories,
    photos,
    config,
  },
  children,
}) => {
  const hasNav = categories.some(c => c.parent
    && c.parent.id === (category.parent || category).id,
  );

  const header = <Header category={category} categories={categories} />;

  return (
    <Main header={header} langs={category.langs}>
      <Helmet>
        <title>{category.title}</title>
        <meta name="description" content={category.description} />
      </Helmet>
      <div
        className={classnames(
          style.gallery,
          hasNav && style.nav,
          // loading && style.loading,
        )}
      >
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
        {children && React.cloneElement(children, {
          config,
          category,
          photos,
        })}
        {/* </CSSTransitionGroup> */}
        <Gallery category={category} photos={photos} config={config} />
      </div>
    </Main>
  );
};

// GalleryPage.propTypes = {
//   children: element,
//   category: shape().isRequired,
//   categories: arrayOf(shape()).isRequired,
//   loading: bool.isRequired,
// };
//
// GalleryPage.defaultProps = {
//   children: null,
// };

export default withStyles(style, transitionStyle)(GalleryPage);
