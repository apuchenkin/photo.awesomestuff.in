import React from 'react';
import {FormattedMessage} from 'react-intl';

import Link from '../link';
import CategoryLink from '../link/category';
import './navigation.less';

const {object, array} = React.PropTypes;

export default class Header extends React.Component {

  static propTypes = {
    category: object.isRequired,
    categories: array.isRequired
  }

  render() {
    const
      props = this.props,
      category = props.category.parent || props.category,
      childrens = props.categories
        .filter(c => c.parent && c.parent.name === category.name)
        .map(c => {
          return (
             <li className="item" key={c.id} >
               <CategoryLink category={c.parent.name} subcategory={c.name}>{c.title}</CategoryLink>
             </li>
          );
        })
      ;

    return (
      <header className="main" ref="main">
        <h1 className="title">
        {[
          <Link to='/' activeClassName="active" key="page.home"><FormattedMessage id="home" defaultMessage={`Home`} /></Link>,
          " / ",
          <CategoryLink category={category.name} key="page.category" >{category.title}</CategoryLink>
        ]}
        </h1>
        {childrens && <nav className="categories"><ul>{childrens}</ul></nav>}
      </header>
    );
  }
}
