import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import PhotoService from '../../service/Photo';

import style from './gallery.less';

const { number, string, shape } = React.PropTypes;

const Brick = (props) => {
  const
    { photo } = props,
    { w, h, src } = photo,
    size = Math.max(w, h),
    url = PhotoService.getSrc(src, { width: size, height: size }, true),
    brickStyle = {
      width: `${w}px`,
      height: `${h}px`,
      backgroundImage: `url(${url})`,
    };

  return (
    <div className={style.brick} style={brickStyle} />
  );
};

Brick.propTypes = {
  photo: shape({
    id: number.isRequired,
    src: string.isRequired,
    ratio: number.isRequired,
    w: number.isRequired,
    h: number.isRequired,
  }),
};

export default withStyles(style)(Brick);
