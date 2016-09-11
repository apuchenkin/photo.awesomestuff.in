import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import withRouter from 'react-router/lib/withRouter';
import Category from '../link/category';
import PhotoLink from '../link/photo';
import CategoryLink from '../link/category';
import resolutions from './resolution.json';
import config from '../../config.json';
import Link from '../link';
import Loader from '../loader';
import './photo.less';
import utils from '../../lib/utils';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';
import {injectIntl, intlShape, FormattedMessage} from 'react-intl';
import { bind, debounce } from 'decko';

const
  isBrowser = (typeof window !== 'undefined'),
  { object, array } = React.PropTypes
  ;

class Photo extends React.Component {

  static propTypes = {
    category: object.isRequired,
    photos: array.isRequired,
    photo: object.isRequired,
    intl: intlShape.isRequired,
    router: object.isRequired
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  constructor(props) {
    super(props);

    this.state = {
      isLoading: true,
      dimensions: this.getDimensions()
    };
  }

  getDimensions() {
    return {
      width: isBrowser ? window.innerWidth - 40 : config.photo.width,
      height: isBrowser ? window.innerHeight - 40 : config.photo.height
    };
  }

  componentDidMount() {
    if (this.refs.img.complete) {
      this.setState({
        isLoading: false
      });
    }

    window.addEventListener('resize', this.resize);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.resize);
  }


  @bind
  @debounce(50)
  resize() {
    this.setState({
      dimensions: this.getDimensions()
    });
  }

  @bind
  adjust (w, h) {
    const
      norms = resolutions.map(([w$,h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
      min = Math.min(...norms),
      idx = norms.findIndex(n => n === min)
      ;

    return resolutions[idx];
  }

  @bind
  close() {
    const
      {category, router} = this.props,
      url = category.parent ? category.parent.name + '/' + category.name : category.name
      ;

    router.push('/' + url);
  }

  @bind
  goNext(next) {
    const
      {category, router} = this.props,
      url = category.parent ? category.parent.name + '/' + category.name : category.name
      ;

    router.push('/' + url + '/photo/' + next.id);
  }

  @bind
  onLoad() {
    this.setState({isLoading: false});
  }

  render() {
    const
      state = this.state,
      {intl, photo, category, photos} = this.props,
      pidx = photos.findIndex(p => p.id === photo.id),
      prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1],
      next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1],
      {width, height} = state.dimensions,
      [w, h] = this.adjust(width, height),
      filename = photo.src.split('/').pop(),
      src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/'),
      url = '/' + (category.parent ? category.parent.name + '/' + category.name : category.name),
      closeIcon = <FormattedMessage
        id="icon.close"
        defaultMessage={`Close {icon}`}
        values={{icon: (<i className="icon-cancel"></i>)}}
      />,
      figure = (
        <figure className={state.isLoading ? "content loading" : "content"} >
          <div className="tools"><Link onClick={e => e.stopPropagation()} to={url}>{closeIcon}</Link></div>
          <img className="photo" onClick={e => {e.stopPropagation(); this.goNext(next);}} src={src} style={{maxWidth: width + 'px', maxHeight: (height - 60) + 'px'}} onLoad={this.onLoad} ref='img' />
          <figcaption className="description">
            <span className="caption">{photo.caption}</span>
            {photo.author && <div><FormattedMessage
              id="photo.author"
              defaultMessage={`Author: {author}`}
              values={{author: (<span className="author">{photo.author.name}</span>)}}
              /></div>}
          </figcaption>
        </figure>
      )
      ;

    return (
      <div className="photo-widget" onClick={this.close}>
        <ReactCSSTransitionGroup transitionName="loader" transitionAppearTimeout={200} transitionEnterTimeout={200} transitionLeaveTimeout={200} transitionAppear={false}>
          {this.state.isLoading && <Loader />}
        </ReactCSSTransitionGroup>
        {figure}
        <PhotoLink
          onClick={e => e.stopPropagation()}
          {...CategoryLink.fromCategory(category)}
          photoId={prev && prev.id}
          className="nav prev"
          title={intl.formatMessage({id: 'prev'})}>
          <i className="icon-left-open" />
        </PhotoLink>
        <PhotoLink onClick={e => e.stopPropagation()}
          {...CategoryLink.fromCategory(category)}
          photoId={next && next.id}
          className="nav next"
          title={intl.formatMessage({id: 'next'})}>
          <i className="icon-right-open" />
        </PhotoLink>
      </div>
    );
  }
}

export default withRouter(injectIntl(Photo));
