import React from 'react';
import { connect } from 'react-redux';
import { FormattedDate } from 'react-intl';
import { shape, arrayOf, string, number } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import CategoryLink, { fromCategory } from '../link/category';
import style from './style.less';
import baseStyle from '../../style/style.less';

const Gallery = ({ category, getCategoryImage, childs, width }) => (
  <div className={style.gallery}>
    <CategoryLink category={category.name} className={style.cover}>
      <img
        src={getCategoryImage(category)}
        width={width}
        title={category.title}
        alt={category.title}
      />
      {category.date && <span className={style.sub}>
        <FormattedDate
          value={category.date}
          year="numeric"
          month="long"
        />
      </span>}
    </CategoryLink>
    <aside>
      <h3>
        <CategoryLink {...fromCategory(category)} className={baseStyle.active}>
          {category.title}
        </CategoryLink>
      </h3>
      {Boolean(childs.length) && (
        <ul>
          {childs.map(c => (
            <li key={c.id}>
              <CategoryLink {...fromCategory(c)}>{c.title}</CategoryLink>
            </li>
          ))}
        </ul>
      )}
    </aside>
  </div>
);

const categoryShape = shape({
  name: string.isRequired,
  title: string.isRequired,
  date: string,
  featured: shape({
    src: string.isRequired,
  }).isRequired,
});

Gallery.propTypes = {
  width: number.isRequired,
  height: number.isRequired,
  category: categoryShape.isRequired,
  childs: arrayOf(shape({
    id: number.isRequired,
    name: string.isRequired,
    title: string.isRequired,
  })),
};

Gallery.defaultProps = {
  childs: [],
};

export default connect(
  ({ runtime: { config: { gallery } } }) => ({
    width: gallery.width,
    height: gallery.height,
    getCategoryImage: category => `/static/${category.featured}/${gallery.width}/${gallery.height}`,
  }),
)(
  withStyles(style, baseStyle)(Gallery),
);
