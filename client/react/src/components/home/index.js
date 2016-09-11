import React from 'react';
import CategoryService from '../../service/Category';
import CategoryLink from '../link/category';
import config from '../../config/config.json';
import {FormattedMessage} from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';
import './style.less';

const {array, object} = React.PropTypes;

class Gallery extends React.Component {
  static propTypes = {
    category: object.isRequired,
    childs: array.isRequired
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      {category, childs} = this.props;

    return (
      <div className="gallery">
        <CategoryLink category={category.name} className="cover">
          <img src={config.apiEndpoint + config.apiPrefix + '/' + category.image} width={config.gallery.width} title={category.title} alt={category.title}/> {category.date && <span className="sub">{category.date}</span>}
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


export default class Home extends React.Component {

  static propTypes = {
    categories: array.isRequired
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const props = this.props,
      categories = props.categories,
      galleries = categories
        .filter(c => !c.parent && c.title && c.image)
        .map(category => (
          <li key={category.id} >
            <Gallery category={category} childs={category.childs.map(cid => categories.find(c => c.id === cid))} />
          </li>
        ))
      ;

    return (
      <div className="galleries">
        <h2>
          <FormattedMessage id="galleries" defaultMessage={`Galleries`}/>
        </h2>
        <ul>{galleries}</ul>
      </div>
    );
  }
}
