import React from 'react';
import { number, string, shape } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './gallery.less';
import utils from '../../lib/utils';

const Brick = ({ photo }) => {
  const { w, h, src } = photo;
  const size = Math.max(w, h);
  const url = utils.getSrc(src, size, size, true);
  const brickStyle = {
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
  }).isRequired,
};

export default withStyles(style)(Brick);
