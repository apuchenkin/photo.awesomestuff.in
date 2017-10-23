import React from 'react';
import { array, arrayOf, shape, number } from 'prop-types';
// import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Home, { style, Header } from '../components/landing';
import Main from './main';

const messages = defineMessages({
  galleries: {
    id: 'galleries',
    defaultMessage: 'Galleries',
  },
});

const Landing = ({ data: { categories } }) => {
  const galleries = categories
    .filter(c => !c.parent && c.title && c.featured)
    .map(category => (
      <li key={category.id} >
        <Home
          category={category}
          childs={categories.filter(c => c.parent && c.parent.id === category.id)}
        />
      </li>
    ))
  ;

  return (
    <Main header={<Header />}>
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
  categories: arrayOf(shape({
    id: number.isRequired,
    childs: array,
  })).isRequired,
};

export default withStyles(style)(Landing);

// connect(
//   ({ category: { categories } }) => ({ categories }),
// )(
//   withStyles(style)(Home),
// );
