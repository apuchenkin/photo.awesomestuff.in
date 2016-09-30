import React from 'react';
import { bind } from 'decko';

import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Component from '../../lib/PureComponent';
import style from './photo.less';
import loaderStyle from '../loader/style.less';
import utils from '../../lib/utils';

const
  { string, number, element } = React.PropTypes;

@withStyles(style, loaderStyle)
export default class Img extends Component {

  static propTypes = {
    src: string.isRequired,
    alt: string.isRequired,
    width: number.isRequired,
    height: number.isRequired,
    tools: element,
    caption: element,
  }

  constructor(props) {
    super(props);

    this.state = {
      isLoading: !!isBrowser,
    };
  }

  componentDidMount() {
    this.createLoader();
  }

  componentWillReceiveProps(props) {
    if (this.props.src !== props.src) {
      this.setState({ isLoading: true });
    }
  }

  componentDidUpdate() {
    if (this.state.isLoading && !this.img) {
      this.createLoader();
    }
  }

  componentWillUnmount() {
    this.destroyLoader();
  }

  @bind
  createLoader() {
    this.destroyLoader();  // We can only have one loader at a time.

    this.img = new Image();
    this.img.onload = this.handleLoad;
    this.img.onerror = this.handleLoad;
    this.img.src = this.props.src;
  }

  @bind
  handleLoad() {
    this.destroyLoader();
    this.setState({ isLoading: false });
  }

  @bind
  destroyLoader() {
    if (this.img) {
      this.img.onload = null;
      this.img.onerror = null;
      this.img = null;
    }
  }

  renderImg() {
    const
      { alt, width, height, tools, caption } = this.props,
      props$ = utils.omit(this.props, ['width', 'height', 'tools', 'caption']);

    return (
      <figure className={style.content} >
        {tools}
        <img
          {...props$}
          className={style.image}
          style={{ maxWidth: `${width}px`, maxHeight: `${height}px` }}
          alt={alt}
        />
        {caption}
      </figure>
    );
  }

  renderPreloader() {
    return <div className={loaderStyle.loader} ><div className={loaderStyle.accent} /></div>;
  }

  render() {
    return this.state.isLoading
      ? this.renderPreloader()
      : this.renderImg()
    ;
  }
}
