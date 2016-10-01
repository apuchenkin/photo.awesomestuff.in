import React from 'react';
import { connect } from 'react-redux';
import Component from '../../lib/PureComponent';
import { fromCategory } from '../link/category';
import PhotoLink from '../link/photo';
import Brick from './brick';

const
  { shape, arrayOf } = React.PropTypes,
  Packery = isBrowser ? require('packery') : null;

const createPackery = (container) => {
  const packery = new Packery(container, {
    columnWidth: 100,
    itemSelector: 'li',
    gutter: 10,
  });

  packery.doUpdate = () => {
    packery.reloadItems();
    packery.layout();
  };

  return packery;
};

class Gallery extends Component {

  static propTypes = {
    category: shape().isRequired,
    photos: arrayOf(shape()).isRequired,
  };

  componentDidMount() {
    this.packery = createPackery(this.packeryCmp);
  }

  componentDidUpdate() {
    if (isBrowser) {
      this.packery.doUpdate();
    }
  }

  componentWillUnmount() {
    this.packery.destroy();
    this.packery = null;
  }

  render() {
    const
      { photos, category } = this.props,
      bricks = photos.map(p => (
        <li key={p.id} >
          <PhotoLink photoId={p.id} {...fromCategory(category)}>
            <Brick photo={p} />
          </PhotoLink>
        </li>
      ));

    return (
      <ul ref={(c) => { this.packeryCmp = c; }}>
        {bricks}
      </ul>
    );
  }
}

export default connect(
  state => ({
    isLoading: state.isLoading.count > 0,
    photos: state.api.photos,
  }))(Gallery);
