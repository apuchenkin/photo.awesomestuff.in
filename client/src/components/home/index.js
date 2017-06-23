import React from 'react';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import Gallery from './gallery';
import style from './style.less';

const { array, arrayOf, shape, number } = React.PropTypes;

const messages = defineMessages({
  galleries: {
    id: 'galleries',
    defaultMessage: 'Galleries',
  },
});

const Home = ({ categories }) => {
  const galleries = categories
    .filter(c => !c.parent && c.title && c.featured)
    .map(category => (
      <li key={category.id} >
        <Gallery
          category={category}
          childs={category.childs.map(cid => categories.find(c => c.id === cid))}
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

export default connect(
  ({ category: { categories } }) => ({ categories }),
)(
  withStyles(style)(Home),
);
