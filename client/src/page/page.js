import React from 'react';
import { Helmet } from 'react-helmet';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import { style, Header } from '../components/page';
import Main from './main';

const Page = ({ data: { page: { title, langs, content, description } } }) => (
  <Main header={<Header title={title} />} langs={langs}>
    <Helmet>
      <title>{title}</title>
      <meta name="description" content={description} />
    </Helmet>
    <div className={style.page} dangerouslySetInnerHTML={{ __html: content }} />
  </Main>
);

export default withStyles(style)(Page);
