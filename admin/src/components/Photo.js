import React from 'react';
import classNames from 'classnames';
import { DragSource, DropTarget } from 'react-dnd';
import { Link, withRouter } from 'react-router-dom';

import photoService from '../../../common/service/photo/memoize';
import config from '../../etc/config';

export const PHOTO = 'photo';

const photoSource = {
  beginDrag({ photo }) {
    return photo;
  },
};

const photoDrop = {
  drop({ parent, photo }, monitor) {
    parent.group([monitor.getItem(), photo]);
  },
  canDrop({ photo }, monitor) {
    return photo.id !== monitor.getItem().id;
  },
};

const collectDrop = (connect, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: connect.dropTarget(),
});

const collectDrag = (connect, monitor) => ({
  dragSource: connect.dragSource(),
  isDragging: monitor.isDragging(),
});

const Group = ({ color, onClick }) => (
  <div
    tabIndex="0"
    role="button"
    className="group"
    style={{ background: color }}
    onClick={onClick}
  />
);

const translateColor = (photo) => {
  if (!photo.translations || !photo.translations.length) {
    return 'red';
  }

  if (photo.translations.find(translation => translation.language === 'ru')
   && photo.translations.find(translation => translation.language === 'en')
  ) {
    return 'green';
  }

  return 'yellow';
};

const getSrc = (...args) => [
  config.staticEndpoint,
  photoService.getSrc(...args),
].join('/');

class Photo extends React.Component {
  render() {
    const { photo, parent, group, match, featured, history } = this.props;

    // These two props are injected by React DnD,
    // as defined by your `collect` function above:
    const isDragging = this.props.isDragging;
    const dragSource = this.props.dragSource;
    const dropTarget = this.props.dropTarget;
    const highlighted = this.props.highlighted;
    const hovered = this.props.hovered;

    return dragSource(dropTarget(
      <div
        className={classNames({
          photo: true,
          'photo--highlighted': highlighted,
          'photo--hovered': hovered,
          dragging: isDragging,
          selected: parent.isSelected(photo),
          hasParent: photo.hasParent,
          isHidden: photo.hidden,
        })}
        onClick={e => parent.select(photo, e.ctrlKey)}
        onDoubleClick={() => history.push(`${match.url}/${photo.id}/translation`)}
        role="presentation"
      >
        <div className="views">{photo.views}</div>
        <Link to={`${match.url}/${photo.id}/translation`} >
          <button className="translation material-icons" style={{ color: translateColor(photo) }}>
            translate
          </button>
        </Link>
        {featured && <div className="featured material-icons">star</div>}
        {photo.hasParent && <div className="parent" />}
        {photo.group && <Group color={group} onClick={() => parent.ungroup(photo)} />}
        <div
          className="img"
          alt={photo.name}
          style={{
            width: 150,
            height: 150,
            backgroundImage: `url(${getSrc(photo.src, 200, 200, true)})`,
            backgroundPosition: 'center center',
            backgroundSize: 'cover',
            backgroundRepeat: 'no-repeat',
          }}
        />
      </div>,
    ));
  }
}

export default DragSource(PHOTO, photoSource, collectDrag)(
  DropTarget(PHOTO, photoDrop, collectDrop)(withRouter(Photo)),
);
