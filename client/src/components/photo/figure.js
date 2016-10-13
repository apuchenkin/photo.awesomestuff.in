import React from 'react';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { bind, debounce } from 'decko';

import { getSrc } from '../../lib/utils';

import Component from '../../lib/PureComponent';
import Link from '../link';

import Img from './img';

import style from './photo.less';

const
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
    width: number.isRequired,
    height: number.isRequired,
    photo: photoShape.isRequired,
    backUrl: string.isRequired,
    onClick: func.isRequired,
  }

  constructor(props) {
    super(props);

    this.width = props.width;
    this.height = props.height;
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
      width: isBrowser ? window.innerWidth - 40 : this.width,
      height: isBrowser ? window.innerHeight - 40 : this.height,
    };
  }

  @bind
  @debounce(50)
  resize() {
    this.setState({
      dimensions: this.getDimensions(),
    });
  }

  renderTools() {
    const { backUrl } = this.props;

    return (
      <div className={style.tools}>
        <Link onClick={e => e.stopPropagation()} to={backUrl}>{closeIcon}</Link>
      </div>
    );
  }

  renderCaption() {
    const { photo } = this.props;

    return (
      <figcaption className={style.description}>
        <span className={style.caption}>{photo.caption}</span>
        {photo.author && <div><FormattedMessage
          {...messages.author}
          values={{ author: (<span className={style.author}>{photo.author.name}</span>) }}
        /></div>}
      </figcaption>
    );
  }

  render() {
    const
      { dimensions } = this.state,
      { photo, onClick } = this.props,
      { width, height } = dimensions,
      src = getSrc(photo.src, width, height);

    return (
      <Img
        alt={photo.caption}
        onClick={(e) => { e.stopPropagation(); onClick(); }}
        src={src}
        width={width}
        height={height - 60}
        tools={this.renderTools()}
        caption={this.renderCaption()}
      />
    );
  }
}

export default connect(
  state => ({
    width: state.runtime.config.photo.width,
    height: state.runtime.config.photo.height,
  })
)(
  withStyles(style)(Figure)
);
