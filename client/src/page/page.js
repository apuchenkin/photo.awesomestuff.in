import React from 'react';
// import { string } from 'prop-types';
// import { connect } from 'react-redux';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import { style, Header } from '../components/page';
import Main from './main';

const Page = ({ data: { page } }) => (
  <Main header={<Header title={page.title} />}>
    <div className={style.page} dangerouslySetInnerHTML={{ __html: page.content }} />
  </Main>
)
;

// Page.propTypes = {
//   content: string.isRequired,
// };

export default withStyles(style)(Page);
// connect(
//   ({ page: { page } }) => ({ content: page.content }),
// )(

// );
