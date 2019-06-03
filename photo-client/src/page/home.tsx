import React, { FunctionComponent } from 'react';
import { Helmet } from 'react-helmet';
import { injectIntl, InjectedIntl } from 'react-intl';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Gallery, { style, Header } from '@app/components/home';
import Main from './main';

interface Props {
  intl: InjectedIntl;
  categories: Category[];
}

const Home: FunctionComponent<Props> = ({ intl, categories }) => {
  const galleries = categories
    .filter(category => Boolean(category.featured))
    .sort((c1, c2) => Number(c2.date) - Number(c1.date))
    .map(category => (
      <li key={category.name} >
        <Gallery category={category} />
      </li>
    ));

  return (
    <Main header={<Header />} className={style.home}>
      <Helmet>`
        <meta name="description" content={intl.formatMessage({ id: 'meta.description' })} />
      </Helmet>
      <ul>{galleries}</ul>
    </Main>
  );
};


export default withStyles(style)(injectIntl(Home));
