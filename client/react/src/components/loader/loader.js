import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './style.less';

const
  { bool } = React.PropTypes,
  CLASS_NAME = style.loader,
  CLASS_NAME_TRANSITION = style.fade,
  CLASS_NAME_HIDDEN = style.hidden,
  TRANSITION_DURATION = 200;

class Loader extends React.Component {

  static propTypes = {
    visible: bool.isRequired,
  }

  constructor(props) {
    super(props);

    this.state = {
      visible: props.visible,
      className: null,
    };
  }

  componentWillReceiveProps(props) {
    if (props.visible) {
      this.setState({
        visible: true,
        className: CLASS_NAME_TRANSITION,
      }, () => this.setState({
        className: null,
      }));
    } else {
      this.setState({
        className: CLASS_NAME_TRANSITION,
      }, () => setTimeout(() => this.setState({
        visible: false,
        className: null,
      }), TRANSITION_DURATION));
    }
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      { visible, className } = this.state,
      className$ = [
        CLASS_NAME,
        visible ? className : CLASS_NAME_HIDDEN,
      ].filter(x => !!x).join(' ');

    return (
      <div className={className$} key="loader"><div className={style.accent} /></div>
    );
  }
}

export default withStyles(style)(Loader);
