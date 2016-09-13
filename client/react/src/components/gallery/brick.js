import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';

import config from '../../config/config.json';

const { number, string, shape } = React.PropTypes;

export default class Brick extends React.Component {

  static propTypes = {
    photo: shape({
      id: number.isRequired,
      src: string.isRequired,
    }),
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      { photo } = this.props,
      { w, h, ratio } = photo,
      inc = ratio >= 1 ? ratio : 1 / ratio,
      [m1, m2] = w < h ? [Math.ceil(w * inc), h] : [Math.ceil(h * inc), w],
      s = Math.max(m1, m2),
      filename = photo.src.split('/').pop(),
      src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, s, s, filename].join('/');

    return (
      <div className="brick" style={{ width: `${w}px`, height: `${h}px`, backgroundImage: `url(${src})` }} />
    );
  }
}
