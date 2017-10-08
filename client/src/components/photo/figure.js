import React from 'react';
import { number, string, object, shape, func } from 'prop-types';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import debounce from 'debounce';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Link from 'found/lib/Link';
import Close from './icons/close';
// import utils from '../../lib/utils';
import Img from './img';
import { setRuntimeVariable } from '../../store/runtime/actions';

import style from './photo.less';

const photoShape = shape({
  id: number.isRequired,
  src: string.isRequired,
  width: number.isRequired,
  height: number.isRequired,
  description: string.isRequired,
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

  static getDimensions() {
    return {
      width: window.innerWidth - 40,
      height: window.innerHeight - 40,
    };
  }

  constructor(props) {
    super(props);

    this.resize = debounce(this.resize, 175).bind(this);
  }

  componentWillMount() {
    const { width, height } = this.props;
    this.props.setRuntimeVariable('dimensions', {
      width,
      height,
    });
  }

  componentDidMount() {
    window.addEventListener('resize', this.resize);
  }

  componentWillUnmount() {
    this.props.setRuntimeVariable('dimensions', null);
    window.removeEventListener('resize', this.resize);
  }

  componentWillReceiveProps(props) {
    if (props.photo !== this.props.photo) {
      this.props.setRuntimeVariable('dimensions', Figure.getDimensions());
    }
  }

  resize() {
    this.props.setRuntimeVariable('dimensions', Figure.getDimensions());
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
        <span className={style.caption}>{photo.description}</span>
        {photo.author && <div><FormattedMessage
          {...messages.author}
          values={{ author: (<span className={style.author}>{photo.author.name}</span>) }}
        /></div>}
      </figcaption>
    );
  }

  render() {
    const { width, height, photo, onClick, getSrc } = this.props;
    const src = getSrc(photo.src, width, height);

    return (
      <Img
        alt={photo.description}
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
  ({ runtime: { config, dimensions } }) => (dimensions
    ? ({ ...dimensions })
    : (isBrowser ? { ...Figure.getDimensions() } : { ...config.photo })
  ),
  {
    setRuntimeVariable,
  },
)(
  withStyles(style)(Figure),
);
