import * as React from 'react';
import Link from 'found/lib/Link';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import VisibilitySensor from 'react-visibility-sensor';
import Brick from './brick';
import style from './gallery.scss';
import { ConfigContext } from '@app/context';
import Nav from './navigation';

interface Props {
  category: Category;
  parent?: Category;
  photos: Photo[];
  bricks: {
    [key: number]: {w: number, h: number}
  }
}

const getSize = (width: number, gutter: number, n: number) => (width * n) + (gutter * (n - 1));

const Gallery: React.FunctionComponent<Props> = ({ category, parent, photos, bricks }) => {
  const { brickWidth, gutter } = React.useContext(ConfigContext);
  const categoryPath = [parent && parent.name, category.name].filter(Boolean).join('/');

  return (
    <>
      <Nav category={parent || category} />
      <ul className={style.gallery}>
        {photos.map(photo => (
          <VisibilitySensor partialVisibility={true} key={photo.id}>
            {({ isVisible }) => (
              <li style={{
                gridColumnEnd: `span ${bricks[photo.id].w}`,
                gridRowEnd: `span ${bricks[photo.id].h}`,
              }} >
                <Link to={`/${categoryPath}/photo/${photo.id}`}>
                  <Brick
                    photo={photo}
                    isVisible={isVisible}
                    size={getSize(brickWidth, gutter, Math.max(bricks[photo.id].w, bricks[photo.id].h))}
                  />
                </Link>
              </li>
            )}
          </VisibilitySensor>
        ))}
      </ul>
    </>
  )
}

export default withStyles(style)(Gallery);
