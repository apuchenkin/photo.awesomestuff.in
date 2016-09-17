import React from 'react';
import { injectIntl, intlShape, FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import withRouter from 'react-router/lib/withRouter';
import { routerShape } from 'react-router/lib/PropTypes';
import { bind, debounce } from 'decko';

import PhotoLink from '../link/photo';
import CategoryLink from '../link/category';
import Link from '../link';
import Loader from '../loader/loader';

import resolutions from '../../config/resolution.json';
import config from '../../config/config.json';

import './photo.less';

const
  isBrowser = (typeof window !== 'undefined'),
  { number, string, object, shape, arrayOf } = React.PropTypes
  ;

const photoShape = shape({
  id: number.isRequired,
  src: string.isRequired,
  width: number.isRequired,
  height: number.isRequired,
  caption: string.isRequired,
  author: object,
});

class Photo extends React.Component {

  static propTypes = {
    category: shape({ name: string.isRequired }).isRequired,
    photos: arrayOf(shape({ id: number.isRequired })).isRequired,
    photo: photoShape.isRequired,
    intl: intlShape.isRequired,
    router: routerShape.isRequired,
  }

  constructor(props) {
    super(props);

    this.state = {
      isLoading: true,
      dimensions: this.getDimensions(),
    };
  }

  componentDidMount() {
    if (this.img.complete) {
      // in the case, when photo is loaded faster than JS code
      // it is already complete on componentDidMount
      this.onMount(() => {
        this.setState({
          isLoading: false,
        });
      });
    }

    window.addEventListener('resize', this.resize);
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
    this.setState({ isLoading: false });
  }

  getDimensions() {
    return {
      width: isBrowser ? window.innerWidth - 40 : config.photo.width,
      height: isBrowser ? window.innerHeight - 40 : config.photo.height,
    };
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
    });
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
      { isLoading, dimensions } = this.state,
      { intl, photo, category, photos } = this.props,
      pidx = photos.findIndex(p => p.id === photo.id),
      prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1],
      next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1],
      { width, height } = dimensions,
      [w, h] = this.adjust(width, height),
      filename = photo.src.split('/').pop(),
      src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/'),
      url = `/${category.parent ? `${category.parent.name}/${category.name}` : category.name}`,
      closeIcon = (<FormattedMessage
        id="icon.close"
        defaultMessage={'Close {icon}'}
        values={{ icon: (<i className="icon-cancel" />) }}
      />),
      figure = (
        <figure className={isLoading ? 'content loading' : 'content'} >
          <div className="tools"><Link onClick={e => e.stopPropagation()} to={url}>{closeIcon}</Link></div>
          {
            // eslint-disable-next-line jsx-a11y/no-static-element-interactions
            <img
              className="photo"
              alt={photo.caption}
              onClick={(e) => { e.stopPropagation(); this.goNext(next); }}
              onLoad={this.onLoad}
              src={src}
              style={{ maxWidth: `${width}px`, maxHeight: `${height - 60}px` }}
              ref={(c) => { this.img = c; }}
            />
          }
          <figcaption className="description">
            <span className="caption">{photo.caption}</span>
            {photo.author && <div><FormattedMessage
              id="photo.author"
              defaultMessage={'Author: {author}'}
              values={{ author: (<span className="author">{photo.author.name}</span>) }}
            /></div>}
          </figcaption>
        </figure>
      )
      ;

    return (
      // eslint-disable-next-line jsx-a11y/no-static-element-interactions
      <div className="photo-widget" onClick={this.close}>
        <Loader visible={isLoading} />
        {figure}
        <PhotoLink
          {...CategoryLink.fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={prev && prev.id}
          className="nav prev"
          title={intl.formatMessage({ id: 'prev' })}
        >
          <i className="icon-left-open" />
        </PhotoLink>
        <PhotoLink
          {...CategoryLink.fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={next && next.id}
          className="nav next"
          title={intl.formatMessage({ id: 'next' })}
        >
          <i className="icon-right-open" />
        </PhotoLink>
      </div>
    );
  }
}

export default withRouter(injectIntl(Photo));
