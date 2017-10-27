import React from 'react';
// import { connect } from 'react-redux';
// import { bool } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
// import { CSSTransition, TransitionGroup } from 'react-transition-group';

import style from './style.less';

// const CLASS_NAME = style.loader;
// const CLASS_NAME_TRANSITION = style.fade;
// const CLASS_NAME_HIDDEN = style.hidden;
// const TRANSITION_DURATION = 200;

// const defaultStyle = {
//   transition: `opacity ${duration}ms ease-in-out`,
//   opacity: 0,
//   padding: 20,
//   display: 'inline-block',
//   backgroundColor: '#8787d8'
// }
//
// const transitionStyles = {
//   entering: { opacity: 0 },
//   entered: { opacity: 1 },
// };

export default withStyles(style)(() => (
  <div className={style.loader}><div className={style.accent} /></div>
));
