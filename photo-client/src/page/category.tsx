import * as React from 'react';
import { Helmet } from 'react-helmet';
import Main from './main';
import Header from '@app/components/gallery/header';
import Gallery from '@app/components/gallery/gallery';
import style from '@app/components/gallery/gallery.scss';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Fade from '@app/components/animation/fade';
import { TransitionGroup } from 'react-transition-group';

interface Props {
  parent?: Category;
  category: Category;
  photos: Photo[];
  bricks: {
    [key: number]: {w: number, h: number}
  },
  children: React.ReactElement;
}

const GalleryPage: React.FunctionComponent<Props> = ({
  children,
  parent,
  category,
  photos,
  bricks,
}) => {
  const header = <Header category={parent || category} />;

  return (
    <Main header={header} className={style.gallery}>
      <TransitionGroup component={null}>
        <Helmet>
          <title>{category.title}</title>
          <meta name="description" content={category.description} />
        </Helmet>
        {children && (
          <Fade enter={false}>
          {
            React.cloneElement(children, {
              category,
              photos,
              parent,
            })
          }
        </Fade>
        )}
        <Gallery
          category={category}
          parent={parent}
          photos={photos}
          bricks={bricks}
        />
      </TransitionGroup>
    </Main>
  );
};

export default withStyles(style)(GalleryPage);
