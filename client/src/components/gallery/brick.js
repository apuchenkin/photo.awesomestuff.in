import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import PhotoService from '../../service/Photo';

import config from '../../config/config.json';
import style from './gallery.less';

const { number, string, shape } = React.PropTypes;

const Brick = (props) => {
  const
    { photo } = props,
    { w, h } = photo,
    s = PhotoService.getSize(photo),
    filename = photo.src.split('/').pop(),
    src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, s, s, filename].join('/');

  return (
    <div className={style.brick} style={{ width: `${w}px`, height: `${h}px`, backgroundImage: `url(${src})` }} />
  );
};

Brick.propTypes = {
  photo: shape({
    id: number.isRequired,
    src: string.isRequired,
    w: number.isRequired,
    h: number.isRequired,
  }),
};

export default withStyles(style)(Brick);
