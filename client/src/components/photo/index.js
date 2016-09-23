import React from 'react';
import { connect } from 'react-redux';
import { injectIntl, intlShape, defineMessages, FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import withRouter from 'react-router/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { routerShape } from 'react-router/lib/PropTypes';
import { bind, debounce } from 'decko';

import { startLoading, stopLoading } from '../../actions/loader';
import PhotoLink from '../link/photo';
import { fromCategory } from '../link/category';
import Link from '../link';

import resolutions from '../../config/resolution.json';
import config from '../../config/config.json';
import style from './photo.less';

const
  isBrowser = (typeof window !== 'undefined'),
  { number, string, object, shape, arrayOf, func, bool } = React.PropTypes
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
  prev: {
    id: 'prev',
    defaultMessage: 'Prev',
  },
  next: {
    id: 'next',
    defaultMessage: 'Next',
  },
});

class Photo extends React.Component {

  static propTypes = {
    category: shape({ name: string.isRequired }).isRequired,
    photos: arrayOf(shape({ id: number.isRequired })).isRequired,
    photo: photoShape.isRequired,
    intl: intlShape.isRequired,
    router: routerShape.isRequired,
    startLoading: func.isRequired,
    stopLoading: func.isRequired,
    isLoading: bool.isRequired,
  }

  constructor(props) {
    super(props);

    const dimensions = this.getDimensions();
    this.state = {
      dimensions,
      src: this.getSrc(props, dimensions),
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

  componentWillReceiveProps(props) {
    const src = this.getSrc(props, this.state.dimensions);
    if (src !== this.state.src) {
      this.setState({ src }, () => this.props.startLoading);
    }
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.resize);
  }

  onMount(callback) {
    callback();
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

  @bind
  getSrc(props, dimensions) {
    const
      { width, height } = dimensions,
      { photo } = props,
      [w, h] = this.adjust(width, height),
      filename = photo.src.split('/').pop(),
      src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/');

    return src;
  }

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
    }, () => this.componentWillReceiveProps(this.props));
  }

  @bind
  goNext(next) {
    const
      { category, router } = this.props,
      url = category.parent ? `${category.parent.name}/${category.name}` : category.name
      ;

    router.push(`/${url}/photo/${next.id}`);
  }

  @bind
  close() {
    const
      { category, router } = this.props,
      url = category.parent ? `${category.parent.name}/${category.name}` : category.name
      ;

    router.push(`/${url}`);
  }

  render() {
    const
      { dimensions, src } = this.state,
      { intl, photo, category, photos, isLoading } = this.props,
      pidx = photos.findIndex(p => p.id === photo.id),
      prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1],
      next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1],
      { width, height } = dimensions,
      url = `/${category.parent ? `${category.parent.name}/${category.name}` : category.name}`,
      closeIcon = (<FormattedMessage
        {...messages.close}
        values={{ icon: (<i className="icon-cancel" />) }}
      />),
      figure = (
        <figure className={isLoading ? `${style.content} ${style.loading}` : style.content} >
          <div className={style.tools}>
            <Link onClick={e => e.stopPropagation()} to={url}>{closeIcon}</Link>
          </div>
          {
            // eslint-disable-next-line jsx-a11y/no-static-element-interactions
            <img
              className={style.image}
              alt={photo.caption}
              onClick={(e) => { e.stopPropagation(); this.goNext(next); }}
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
      )
      ;

    return (
      // eslint-disable-next-line jsx-a11y/no-static-element-interactions
      <div className={style.photo} onClick={this.close}>
        {figure}
        <PhotoLink
          {...fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={prev && prev.id}
          className={`${style.nav} ${style.prev}`}
          title={intl.formatMessage(messages.prev)}
        >
          <i className="icon-left-open" />
        </PhotoLink>
        <PhotoLink
          {...fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={next && next.id}
          className={`${style.nav} ${style.next}`}
          title={intl.formatMessage(messages.next)}
        >
          <i className="icon-right-open" />
        </PhotoLink>
      </div>
    );
  }
}

export default connect(
  state => ({ isLoading: state.isLoading.count > 0 }),
  dispatch => ({
    startLoading: () => dispatch(startLoading()),
    stopLoading: () => dispatch(stopLoading()),
  })
)(withStyles(style)(withRouter(injectIntl(Photo))));
