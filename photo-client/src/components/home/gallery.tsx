import React, { FunctionComponent } from 'react';
import { FormattedDate } from 'react-intl';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Link from 'found/lib/Link';
import style from './home.scss';
import baseStyle from '@app/style.scss';
import { getThumb } from '@app/service/photo';

interface Props {
  category: Category;
}

const GALLERY_WIDTH = 320;
const GALLERY_HEIGHT = 240;

const Gallery: FunctionComponent<Props> = ({ category }) => (
  <div className={style.gallery}>
    <h3>
      <Link to={`/${category.name}`} className={baseStyle.active}>
        {category.title}
      </Link>
    </h3>
    <Link to={`/${category.name}`} activeClassName={baseStyle.active} className={style.cover}>
      {category.featured && (
        <img
          src={getThumb(GALLERY_WIDTH, category.featured)}
          width={GALLERY_WIDTH}
          height={GALLERY_HEIGHT}
          title={category.title}
          alt={category.title}
        />
      )}
      {category.date && <span className={style.sub}>
        <FormattedDate
          value={category.date}
          year="numeric"
          month="long"
        />
      </span>}
    </Link>
    <aside>
      {Boolean(category.children.length) && (
        <ul>
          {category.children.map(child => (
            <li key={child.name}>
              <Link to={`/${category.name}/${child.name}`}>{child.title}</Link>
            </li>
          ))}
        </ul>
      )}
    </aside>
  </div>
);

export default withStyles(style, baseStyle)(Gallery);
