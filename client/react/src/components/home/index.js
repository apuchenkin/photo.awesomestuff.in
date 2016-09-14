import React from 'react';
import { FormattedMessage } from 'react-intl';
import shallowCompare from 'react-addons-shallow-compare';

import Gallery from './gallery';

import './style.less';

const { array, arrayOf, shape, number } = React.PropTypes;

export default class Home extends React.Component {

  static propTypes = {
    categories: arrayOf(shape({
      id: number.isRequired,
      childs: array,
    })).isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    const
      { categories } = this.props,
      galleries = categories
        .filter(c => !c.parent && c.title && c.image)
        .map(category => (
          <li key={category.id} >
            <Gallery
              category={category}
              childs={category.childs.map(cid => categories.find(c => c.id === cid))}
            />
          </li>
        ))
      ;

    return (
      <div className="galleries">
        <h2>
          <FormattedMessage id="galleries" defaultMessage={'Galleries'} />
        </h2>
        <ul>{galleries}</ul>
      </div>
    );
  }
}
