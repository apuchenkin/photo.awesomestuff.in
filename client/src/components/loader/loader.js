import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './style.less';

export default withStyles(style)(() => (
  <div className={style.loader}>
    <div className={style.accent} />
  </div>
));
