import React from 'react';
import { string, number, element } from 'prop-types';
import ImageLoader from 'react-imageloader';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import style from './photo.less';
import Loader from '../loader';

const Img = ({
  src,
  alt,
  width,
  height,
  tools,
  caption,
  ...props
}) => (
  <figure className={style.content} >
    {tools}
    <ImageLoader
      src={src}
      imgProps={{
        alt,
        ...props,
      }}
      // wrapper={React.DOM.figure}
      preloader={() => <Loader />}
      className={style.image}
      style={{ maxWidth: `${width}px`, maxHeight: `${height}px` }}
    />
    {caption}
  </figure>
);

Img.propTypes = {
  src: string.isRequired,
  alt: string.isRequired,
  width: number.isRequired,
  height: number.isRequired,
  tools: element,
  caption: element,
}

export default withStyles(style)(Img);
