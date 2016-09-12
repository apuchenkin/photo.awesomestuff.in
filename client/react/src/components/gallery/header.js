import React from 'react';
import { FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';

import Link from '../link';
import CategoryLink from '../link/category';
import './navigation.less';

const
  { int, string, shape, arrayOf } = React.PropTypes,
  categoryBase = {
    id: int.isRequired(),
    name: string.isRequired(),
    title: string.isRequired(),
  },
  categoryShape = shape(Object.assing(categoryBase, {
    parent: shape(categoryBase),
  }));

export default class Header extends React.Component {

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
          <li className="item" key={c.id} >
            <CategoryLink category={c.parent.name} subcategory={c.name}>{c.title}</CategoryLink>
          </li>
          )
        )
      ;

    return (
      <header className="main">
        <h1 className="title">
        {[
          <Link to="/" activeClassName="active" key="page.home"><FormattedMessage id="home" defaultMessage={'Home'} /></Link>,
          ' / ',
          <CategoryLink category={parent.name} key="page.category" >{parent.title}</CategoryLink>,
        ]}
        </h1>
        {childrens && <nav className="categories"><ul>{childrens}</ul></nav>}
      </header>
    );
  }
}
