import React from 'react';
import { connect } from 'react-redux';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './style.less';

const { string } = React.PropTypes;

const Page = ({ content }) =>
  // eslint-disable-next-line react/no-danger
  <div className={style.page} dangerouslySetInnerHTML={{ __html: content }} />
;

Page.propTypes = {
  content: string.isRequired,
};

export default connect(
  ({ page: { page } }) => ({ content: page.content }),
)(
  withStyles(style)(Page),
);
