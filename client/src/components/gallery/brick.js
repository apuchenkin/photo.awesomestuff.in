import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import PhotoService from '../../service/Photo';
import { apiEndpoint, apiPrefix } from '../../config/config.json';
import style from './gallery.less';

const { number, string, shape } = React.PropTypes;

const Brick = (props) => {
  const
    { photo } = props,
    { w, h, ratio } = photo,
    size = PhotoService.getSize(w, h, ratio),
    filename = photo.src.split('/').pop(),
    src = [apiEndpoint + apiPrefix, 'hs/photo', photo.id, size, size, filename].join('/'),
    brickStyle = {
      width: `${w}px`,
      height: `${h}px`,
      backgroundImage: `url(${src})`,
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
