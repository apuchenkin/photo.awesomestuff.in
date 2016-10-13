import React from 'react';
import { connect } from 'react-redux';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import CategoryLink, { fromCategory } from '../link/category';
import utils from '../../lib/utils';

import style from './style.less';
import baseStyle from '../../style/style.less';

const
  { shape, arrayOf, string, number } = React.PropTypes,
  categoryShape = shape({
    name: string.isRequired,
    title: string.isRequired,
    date: string,
    image: string,
  });

const Gallery = ({ category, childs, width, height }) =>
  <div className={style.gallery}>
    <CategoryLink category={category.name} className={style.cover}>
      <img
        src={utils.getSrc(category.image, width, height, true)}
        width={width}
        title={category.title}
        alt={category.title}
      />
      {category.date && <span className={style.sub}>{category.date}</span>}
    </CategoryLink>
    <aside>
      <h3>
        <CategoryLink {...fromCategory(category)} className={baseStyle.active}>
          {category.title}
        </CategoryLink>
      </h3>
      {childs && !!childs.length && <ul>
        {childs.map(c => (
          <li key={c.id}>
            <CategoryLink {...fromCategory(c)}>{c.title}</CategoryLink>
          </li>
        ))}
      </ul>}
    </aside>
  </div>
;

Gallery.propTypes = {
  width: number.isRequired,
  height: number.isRequired,
  category: categoryShape.isRequired,
  childs: arrayOf(shape({
    id: number.isRequired,
    name: string.isRequired,
    title: string.isRequired,
  })).isRequired,
};

export default connect(
  state => ({
    width: state.runtime.config.gallery.width,
    height: state.runtime.config.gallery.height,
  })
)(
  withStyles(style, baseStyle)(Gallery)
);
