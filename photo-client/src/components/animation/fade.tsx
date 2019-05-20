import * as React from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import { CSSTransition } from 'react-transition-group';
import style from './fade.scss';

const Fade: React.FunctionComponent = ({ children, ...props }) => (
  <CSSTransition
    {...props}
    timeout={175}
    classNames={{
     exit: style.exit,
     exitActive: style.exitActive,
    }}
  >
    {children}
  </CSSTransition>
);

export default withStyles(style)(Fade);
