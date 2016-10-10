import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import CategoryLink, { fromCategory } from '../link/category';
import config from '../../config/config.json';
import PhotoService from '../../service/Photo';

import style from './style.less';
import baseStyle from '../../style/style.less';

const
  { shape, arrayOf, string, number } = React.PropTypes,
  getSrc = category => PhotoService.getSrc(
    category.image,
    { width: config.gallery.width, height: config.gallery.height },
    true
  ),
  categoryShape = shape({
    name: string.isRequired,
    title: string.isRequired,
    date: string,
    image: string,
  });

const Gallery = ({ category, childs }) =>
  <div className={style.gallery}>
    <CategoryLink category={category.name} className={style.cover}>
      <img
        src={getSrc(category)}
        width={config.gallery.width}
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
  category: categoryShape.isRequired,
  childs: arrayOf(shape({
    id: number.isRequired,
    name: string.isRequired,
    title: string.isRequired,
  })).isRequired,
};

export default withStyles(style, baseStyle)(
  Gallery
);
