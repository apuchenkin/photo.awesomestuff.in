import React from 'react';
import { number, string, shape, arrayOf } from 'prop-types';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from 'found/lib/Link';
import CategoryLink from '../link/category';

import style from '../../style/header.less';
import navStyle from './navigation.less';

const categoryBase = {
  id: number.isRequired,
  name: string.isRequired,
  title: string.isRequired,
};

const categoryShape = shape({
  ...categoryBase,
  parent: shape({
    id: number.isRequired,
    name: string.isRequired,
  }),
});

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

const GalleryHeader = ({ category, categories }) => {
  const parent = categories.find(c => c.id === (category.parent || category).id);
  const childrens = categories
    .filter(c => c.parent && c.parent.id === parent.id)
    .map(c => (
      <li key={c.id} >
        <CategoryLink
          activeClassName={navStyle.active}
          category={c.parent.name}
          subcategory={c.name}
        >{c.title}</CategoryLink>
      </li>
    ))
  ;

  return (
    <header className={style.main}>
      <h1 className={style.title}>
        {[
          <Link to="/" key="page.home"><FormattedMessage {...messages.home} /></Link>,
          ' / ',
          <CategoryLink category={parent.name} key="page.category" >{parent.title}</CategoryLink>,
        ]}
      </h1>
      {childrens && <nav className={navStyle.categories}><ul>{childrens}</ul></nav>}
    </header>
  );
};

GalleryHeader.propTypes = {
  category: categoryShape.isRequired,
  categories: arrayOf(categoryShape).isRequired,
};

export default connect(
  ({ category: { category, categories } }) => ({
    category,
    categories,
  }),
)(
  withStyles(navStyle, style)(GalleryHeader),
);
