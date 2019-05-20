import * as React from 'react';
import { Helmet } from 'react-helmet';
import Main from './main';
import { Header } from '@app/components/page';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import style from './style.scss';

interface Props {
  page: Page;
}

const Page: React.FunctionComponent<Props> = ({ page }) => (
  <Main
    langs={page.langs}
    header={<Header title={page.title} />}
  >
    <Helmet>
      <title>{page.title}</title>
      <meta name="description" content={page.description} />
    </Helmet>
    <div
      className={style.page}
      dangerouslySetInnerHTML={{ __html: page.content }}
    />
  </Main>
);

export default withStyles(style)(Page);