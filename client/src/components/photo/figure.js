import React from 'react';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { bind, debounce, memoize } from 'decko';

import Component from '../../lib/PureComponent';
import { startLoading, stopLoading } from '../../actions/loader';
import Link from '../link';

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
  values={{ icon: (<i className="icon-cancel" />) }}
/>);

class Figure extends Component {

  static propTypes = {
    photo: photoShape.isRequired,
    backUrl: string.isRequired,
    onClick: func.isRequied,
    startLoading: func.isRequired,
    stopLoading: func.isRequired,
  }

  constructor(props) {
    super(props);

    this.state = {
      dimensions: this.getDimensions(),
    };
  }

  componentWillMount() {
    this.props.startLoading();
  }

  componentDidMount() {
    if (this.img.complete) {
      // in the case, when photo is loaded faster than JS code
      // it is already complete on componentDidMount
      this.props.stopLoading();
    }

    window.addEventListener('resize', this.resize);
  }

  // componentWillReceiveProps(props) {
    // const src = this.getSrc(props, this.state.dimensions);
    // if (src !== this.state.src) {
    //   this.setState({ src }, () => this.props.startLoading);
    // }
  // }

  componentWillUnmount() {
    window.removeEventListener('resize', this.resize);
  }

  @bind
  onLoad() {
    this.props.stopLoading();
  }

  getDimensions() {
    return {
      width: isBrowser ? window.innerWidth - 40 : config.photo.width,
      height: isBrowser ? window.innerHeight - 40 : config.photo.height,
    };
  }

  @memoize
  @bind
  getSrc(photo, dimensions) {
    const
      { width, height } = dimensions,
      [w, h] = this.adjust(width, height),
      filename = photo.src.split('/').pop(),
      src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/');

    return src;
  }

  @memoize
  @bind
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
    // , () => this.componentWillReceiveProps(this.props));
  }

  render() {
    const
      { dimensions } = this.state,
      { photo, backUrl, onClick } = this.props,
      { width, height } = dimensions,
      src = this.getSrc(photo, dimensions);

    return (
      <figure className={style.content} >
        <div className={style.tools}>
          <Link onClick={e => e.stopPropagation()} to={backUrl}>{closeIcon}</Link>
        </div>
        {
          // eslint-disable-next-line jsx-a11y/no-static-element-interactions
          <img
            className={style.image}
            alt={photo.caption}
            onClick={(e) => { e.stopPropagation(); onClick(); }}
            onLoad={this.onLoad}
            src={src}
            style={{ maxWidth: `${width}px`, maxHeight: `${height - 60}px` }}
            ref={(c) => { this.img = c; }}
          />
        }
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

export default connect(
  state => ({ isLoading: state.isLoading.count > 0 }),
  dispatch => ({
    startLoading: () => dispatch(startLoading()),
    stopLoading: () => dispatch(stopLoading()),
  })
)(withStyles(style)(Figure));
