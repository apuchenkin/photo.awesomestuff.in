import React from 'react';
import { array, arrayOf, shape, number } from 'prop-types';
import { Helmet } from 'react-helmet';
import { injectIntl, intlShape, defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Home, { style, Header } from '../components/landing';
import Main from './main';

const messages = defineMessages({
  galleries: {
    id: 'galleries',
    defaultMessage: 'Galleries',
  },
});

const Landing = ({ intl, data: { categories } }) => {
  const galleries = categories
    .filter(c => !c.parent && c.title && c.featured)
    .sort((c1, c2) => c2.date > c1.date)
    .map(category => (
      <li key={category.id} >
        <Home
          category={category}
          childs={categories.filter(c => c.parent && c.parent.id === category.id)}
        />
      </li>
    ));

  return (
    <Main header={<Header />}>
      <Helmet>
        <meta name="description" content={intl.formatMessage({ id: 'meta.description' })} />
      </Helmet>
      <div className={style.galleries}>
        <h2>
          <FormattedMessage {...messages.galleries} />
        </h2>
        <ul>{galleries}</ul>
      </div>
    </Main>
  );
};

Landing.propTypes = {
  intl: intlShape.isRequired,
  data: shape({
    categories: arrayOf(shape({
      id: number.isRequired,
      childs: array,
    })).isRequired,
  }).isRequired,
};

export default withStyles(style)(injectIntl(Landing));
