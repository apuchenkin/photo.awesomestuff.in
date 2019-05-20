import * as React from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Link from 'found/lib/Link';

import navStyle from './navigation.scss';

interface Props {
  category: Category;
}

const Nav: React.FunctionComponent<Props> = ({ category }) => {
  const childs = category.children.map(child => (
    <li key={child.name}>
      <Link
        activeClassName={navStyle.active}
        to={`/${category.name}/${child.name}`}
      >
        {child.title}
      </Link>
    </li>
  ))

  return childs && childs.length
    ? <nav className={navStyle.categories}><ul>{childs}</ul></nav>
    : null
}

export default withStyles(navStyle)(Nav);