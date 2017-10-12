import React from 'react';
import { array, arrayOf, shape, number } from 'prop-types';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Gallery from '../components/home/gallery';
import style from '../components/home/style.less';

const messages = defineMessages({
  galleries: {
    id: 'galleries',
    defaultMessage: 'Galleries',
  },
});

const Home = ({ data: { categories } }) => {
  const galleries = categories
    .filter(c => !c.parent && c.title && c.featured)
    .map(category => (
      <li key={category.id} >
        <Gallery
          category={category}
          childs={categories.filter(c => c.parent && c.parent.id === category.id)}
        />
      </li>
    ))
  ;

  return (
    <div className={style.galleries}>
      <h2>
        <FormattedMessage {...messages.galleries} />
      </h2>
      <ul>{galleries}</ul>
    </div>
  );
};

Home.propTypes = {
  categories: arrayOf(shape({
    id: number.isRequired,
    childs: array,
  })).isRequired,
};

export default Home;

// connect(
//   ({ category: { categories } }) => ({ categories }),
// )(
//   withStyles(style)(Home),
// );
