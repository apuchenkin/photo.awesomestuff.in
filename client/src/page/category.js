import React from 'react';
import { shape, element, arrayOf, bool } from 'prop-types';
import classnames from 'classnames';
import { TransitionGroup } from 'react-transition-group';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { Helmet } from 'react-helmet';
import Fade from '../components/animation/fade';

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

  const langs = children ? children.props.data.photo.langs : category.langs;
  const header = <Header category={category} categories={categories} />;

  return (
    <Main header={header} langs={langs}>
      <Helmet>
        <title>{category.title}</title>
        <meta name="description" content={category.description} />
      </Helmet>
      <TransitionGroup
        className={classnames(
          style.gallery,
          hasNav && style.nav,
        )}
      >
        {children && React.Children.map(children, child => (
          <Fade enter={false} key={child.props.key}>
            {
              React.cloneElement(children, {
                config,
                category,
                photos,
              })
            }
          </Fade>
        ))}
        <Fade key="gallery"><Gallery category={category} photos={photos} config={config} /></Fade>
      </TransitionGroup>
    </Main>
  );
};

export default withStyles(style, transitionStyle)(GalleryPage);
