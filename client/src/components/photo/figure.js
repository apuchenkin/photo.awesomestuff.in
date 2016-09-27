import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { bind, debounce, memoize } from 'decko';

import Component from '../../lib/PureComponent';
import Link from '../link';

import Img from './img';
import resolutions from '../../config/resolution.json';
import config from '../../config/config.json';

import style from './photo.less';

const
  isBrowser = (typeof window !== 'undefined'),
  { number, string, object, shape, func } = React.PropTypes
  ;

const photoShape = shape({
  id: number.isRequired,
  src: string.isRequired,
  width: number.isRequired,
  height: number.isRequired,
  caption: string.isRequired,
  author: object,
});

const messages = defineMessages({
  close: {
    id: 'icon.close',
    defaultMessage: 'Close {icon}',
  },
  author: {
    id: 'photo.author',
    defaultMessage: 'Author: {author}',
  },
});

const closeIcon = (<FormattedMessage
  {...messages.close}
  values={{ icon: <i className="icon-cancel" /> }}
/>);

class Figure extends Component {

  static propTypes = {
    photo: photoShape.isRequired,
    backUrl: string.isRequired,
    onClick: func.isRequired,
  }

  constructor(props) {
    super(props);

    this.state = {
      dimensions: this.getDimensions(),
    };
  }

  componentDidMount() {
    window.addEventListener('resize', this.resize);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.resize);
  }

  getDimensions() {
    return {
      width: isBrowser ? window.innerWidth - 40 : config.photo.width,
      height: isBrowser ? window.innerHeight - 40 : config.photo.height,
    };
  }


  @bind
  // @memoize --bug??
  getSrc(id, src, dimensions) {
    const
      { width, height } = dimensions,
      [w, h] = this.adjust(width, height),
      filename = src.split('/').pop();

    return [config.apiEndpoint + config.apiPrefix, 'hs/photo', id, w, h, filename].join('/');
  }

  @bind
  @memoize
  adjust(w, h) {
    const
      norms = resolutions.map(([w$, h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
      min = Math.min(...norms),
      idx = norms.findIndex(n => n === min)
      ;

    return resolutions[idx];
  }

  @bind
  @debounce(50)
  resize() {
    this.setState({
      dimensions: this.getDimensions(),
    });
  }

  render() {
    const
      { dimensions } = this.state,
      { photo, backUrl, onClick } = this.props,
      { width, height } = dimensions,
      src = this.getSrc(photo.id, photo.src, dimensions);

    return (
      <figure className={style.content} >
        <div className={style.tools}>
          <Link onClick={e => e.stopPropagation()} to={backUrl}>{closeIcon}</Link>
        </div>
        <Img
          className={style.image}
          alt={photo.caption}
          onClick={(e) => { e.stopPropagation(); onClick(); }}
          src={src}
          width={width}
          height={height - 60}
        />
        <figcaption className={style.description}>
          <span className={style.caption}>{photo.caption}</span>
          {photo.author && <div><FormattedMessage
            {...messages.author}
            values={{ author: (<span className={style.author}>{photo.author.name}</span>) }}
          /></div>}
        </figcaption>
      </figure>
    );
  }
}

export default withStyles(style)(Figure);
