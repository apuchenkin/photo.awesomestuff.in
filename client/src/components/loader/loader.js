import React from 'react';
import { connect } from 'react-redux';
import { bool } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './style.less';

const CLASS_NAME = style.loader;
const CLASS_NAME_TRANSITION = style.fade;
const CLASS_NAME_HIDDEN = style.hidden;
const TRANSITION_DURATION = 200;

class Loader extends React.PureComponent {

  constructor(props) {
    super(props);

    this.state = {
      visible: props.isLoading,
      className: null,
    };
  }

  componentWillReceiveProps(props) {
    if (true) {
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

  render() {
    const { visible, className } = this.state;
    const className$ = [
      CLASS_NAME,
      visible ? className : CLASS_NAME_HIDDEN,
    ].filter(x => !!x).join(' ');

    return (
      <div className={className$} key="loader"><div className={style.accent} /></div>
    );
  }
}

Loader.propTypes = {
  isLoading: bool.isRequired,
};

export default connect(
  state => ({ isLoading: state.isLoading.count > 0 }),
)(withStyles(style)(Loader));
