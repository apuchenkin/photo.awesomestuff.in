import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';

import CategoryLink from '../link/category';
import config from '../../config/config.json';

const
  { shape, arrayOf, string, number } = React.PropTypes,
  categoryShape = shape({
    name: string.isRequired,
    title: string.isRequired,
    date: string,
    image: string,
  });

export default class Gallery extends React.Component {
  static propTypes = {
    category: categoryShape.isRequired,
    childs: arrayOf(shape({
      id: number.isRequired,
      name: string.isRequired,
      title: string.isRequired,
    })).isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      { category, childs } = this.props;

    return (
      <div className="gallery">
        <CategoryLink category={category.name} className="cover">
          <img src={`${config.apiEndpoint}${config.apiPrefix}/${category.image}`} width={config.gallery.width} title={category.title} alt={category.title} />
          {category.date && <span className="sub">{category.date}</span>}
        </CategoryLink>
        <aside>
          <h3>
            <CategoryLink {...CategoryLink.fromCategory(category)} className="active">
              {category.title}
            </CategoryLink>
          </h3>
          {childs && !!childs.length && <ul>
            {childs.map(c => (
              <li key={c.id}>
                <CategoryLink {...CategoryLink.fromCategory(c)}>{c.title}</CategoryLink>
              </li>
            ))}
          </ul>}
        </aside>
      </div>
    );
  }
}
