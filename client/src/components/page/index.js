import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './style.less';

const { string } = React.PropTypes;

const Page = props =>
  // eslint-disable-next-line react/no-danger
  <div className={style.page} dangerouslySetInnerHTML={{ __html: props.content }} />
;

Page.propTypes = {
  content: string.isRequired,
};

export default withStyles(style)(Page);
