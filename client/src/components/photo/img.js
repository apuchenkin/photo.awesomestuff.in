import React from 'react';
import { string, number, element } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import style from './photo.less';
import loaderStyle from '../loader/style.less';

class Img extends React.PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: !!isBrowser,
    };

    this.createLoader = this.createLoader.bind(this);
    this.handleLoad = this.handleLoad.bind(this);
    this.destroyLoader = this.destroyLoader.bind(this);
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

  createLoader() {
    this.destroyLoader();  // We can only have one loader at a time.

    this.img = new Image();
    this.img.onload = this.handleLoad;
    this.img.onerror = this.handleLoad;
    this.img.src = this.props.src;
  }

  handleLoad() {
    this.destroyLoader();
    this.setState({ isLoading: false });
  }

  destroyLoader() {
    if (this.img) {
      this.img.onload = null;
      this.img.onerror = null;
      this.img = null;
    }
  }

  renderImg() {
    const { alt, width, height, tools, caption, ...props } = this.props;

    return (
      <figure className={style.content} >
        {tools}
        <img
          alt={alt}
          className={style.image}
          style={{ maxWidth: `${width}px`, maxHeight: `${height}px` }}
          {...props}
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


Img.propTypes = {
  src: string.isRequired,
  alt: string.isRequired,
  width: number.isRequired,
  height: number.isRequired,
  tools: element,
  caption: element,
}

export default withStyles(style, loaderStyle)(Img)
