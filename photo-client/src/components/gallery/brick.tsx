import * as React from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import style from './gallery.scss';
import { getThumb } from '@app/service/photo';

interface Props {
  size: number;
  photo: Photo;
  isVisible: Boolean;
}

const Brick: React.FunctionComponent<Props> = ({ size, photo, isVisible }) => {
  const src = getThumb(size, photo.src);
  const style$ = isVisible ? {
    backgroundImage: `url(${src})`
  } : {}

  return <div className={style.brick} style={style$} />
};

export default withStyles(style)(Brick);
