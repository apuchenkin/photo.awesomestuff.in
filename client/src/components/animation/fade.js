import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { CSSTransition } from 'react-transition-group';
import style from './fade.less';

const Fade = ({ children, ...props }) => (
  <CSSTransition
    {...props}
    timeout={150}
    classNames={{
     enter: style.enter,
     enterActive: style.enterActive,
     exit: style.exit,
     exitActive: style.exitActive,
    }}
  >
    {children}
  </CSSTransition>
);

export default withStyles(style)(Fade);
