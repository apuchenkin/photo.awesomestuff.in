import React from 'react';
import { shape, arrayOf } from 'prop-types';
// import { connect } from 'react-redux';
import { fromCategory } from '../link/category';
import PhotoLink from '../link/photo';
import Brick from './brick';

const Packery = isBrowser ? require('packery') : null;

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

class Gallery extends React.PureComponent {
  componentDidMount() {
    this.packery = createPackery(this.root);
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
    const { photos, category, config } = this.props;
    const bricks = photos.map(p => (
      <li key={p.id} >
        <PhotoLink photoId={p.id} {...fromCategory(category)}>
          <Brick photo={p} staticEndpoint={config.staticEndpoint} />
        </PhotoLink>
      </li>
    ));

    return (
      <ul ref={(node) => { this.root = node; }}>
        {bricks}
      </ul>
    );
  }
}

Gallery.propTypes = {
  category: shape().isRequired,
  photos: arrayOf(shape()).isRequired,
};

export default Gallery;
