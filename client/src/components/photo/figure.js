import React from 'react';
import { number, string, object, shape, func } from 'prop-types';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Link from 'found/lib/Link';
import Close from './icons/close';
import utils from '../../lib/utils';
import Img from './img';

import style from './photo.less';

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
    defaultMessage: 'Close',
  },
  author: {
    id: 'photo.author',
    defaultMessage: 'Author: {author}',
  },
});

class Figure extends React.PureComponent {

  constructor(props) {
    super(props);

    this.width = props.width;
    this.height = props.height;
    this.state = {
      dimensions: this.getDimensions(),
    };
    this.getDimensions = this.getDimensions.bind(this);
    this.resize = this.resize.bind(this); // TODO: debouce;
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

  resize() {
    this.setState({
      dimensions: this.getDimensions(),
    });
  }

  renderTools() {
    const { backUrl } = this.props;

    return (
      <div className={style.tools}>
        <Link onClick={e => e.stopPropagation()} to={backUrl}>
          <FormattedMessage
            {...messages.close}
          /><Close />
        </Link>
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
    const { dimensions: { width, height } } = this.state;
    const { photo, onClick } = this.props;
    const src = utils.getSrc(photo.src, width, height);

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

Figure.propTypes = {
  width: number.isRequired,
  height: number.isRequired,
  photo: photoShape.isRequired,
  backUrl: string.isRequired,
  onClick: func.isRequired,
};

export default connect(
  state => ({
    width: state.runtime.config.photo.width,
    height: state.runtime.config.photo.height,
  }),
)(
  withStyles(style)(Figure),
);
