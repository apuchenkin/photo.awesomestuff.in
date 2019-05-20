import React from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';

import style from './style.scss';

export default withStyles(style)(() => (
  <div className={style.loader}>
    <div className={style.accent} />
  </div>
));
