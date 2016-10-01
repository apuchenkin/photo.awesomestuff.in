import React from 'react';
import { connect } from 'react-redux';
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

export default connect(
  state => ({ content: state.api.page.content })
)(
  withStyles(style)(Page)
);
