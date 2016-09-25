import React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Link from '../link';
import CategoryLink from '../link/category';

import style from '../../style/header.less';
import navStyle from './navigation.less';

const
  { number, string, shape, arrayOf } = React.PropTypes,
  categoryBase = {
    id: number.isRequired,
    name: string.isRequired,
    title: string.isRequired,
  },
  categoryShape = shape(Object.assign({}, categoryBase, {
    parent: shape(categoryBase),
  }));

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

class GalleryHeader extends React.Component {

  static propTypes = {
    category: categoryShape.isRequired,
    categories: arrayOf(categoryShape).isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      { category, categories } = this.props,
      parent = category.parent || category,
      childrens = categories
        .filter(c => c.parent && c.parent.name === parent.name)
        .map(c => (
          <li key={c.id} >
            <CategoryLink
              activeClassName={navStyle.active}
              category={c.parent.name}
              subcategory={c.name}
            >{c.title}</CategoryLink>
          </li>
          )
        )
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
  }
}

export default withStyles(navStyle, style)(GalleryHeader);
