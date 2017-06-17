import React from 'react';
import classNames from 'classnames';
import { DragSource, DropTarget } from 'react-dnd';
import { Link, withRouter } from 'react-router-dom';

import utils from '../../../client/src/lib/utils';

export const PHOTO = 'photo';

const photoSource = {
  beginDrag({ photo }) {
    return photo;
  },
};

const photoDrop = {
  drop({ parent, photo }, monitor) {
    parent.group([monitor.getItem(), photo])();
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
  // Call this function inside render()
  // to let React DnD handle the drag events:
  dragSource: connect.dragSource(),
  // You can ask the monitor about the current drag state:
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

const translateColor = (category) => {
  if (!category.translations.length) {
    return 'red';
  }

  if (category.translations.find(translation => translation.language === 'ru')
   && category.translations.find(translation => translation.language === 'en')
  ) {
    return 'green';
  }

  return 'yellow';
};

class Photo extends React.Component {
  render() {
    const { photo, parent, group, match } = this.props;

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
        onDoubleClick={() => console.log(photo)}
        role="presentation"
      >
        <div className="views">{photo.views}</div>
        <Link to={`${match.url}/${photo.id}/translation`} >
          <button className="translation material-icons" style={{ color: translateColor(photo) }}>
            translate
          </button>
        </Link>
        {photo.hasParent && <div className="parent" />}
        {photo.group && <Group color={group} onClick={parent.ungroup(photo)} />}
        <img alt={photo.name} src={utils.getSrc(photo.src, 200, 200, true)} />
      </div>,
    ));
  }
}

export default DragSource(PHOTO, photoSource, collectDrag)(
  DropTarget(PHOTO, photoDrop, collectDrop)(withRouter(Photo)),
);
