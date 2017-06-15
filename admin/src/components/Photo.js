import React from 'react';
import classNames from 'classnames';
import { DragSource, DropTarget } from 'react-dnd';

// import config from '../../../client/src/etc/config.json';
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

class Photo extends React.Component {
  render() {
    const { photo, parent, group } = this.props;

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
        {photo.hasParent && <div className="parent" />}
        {photo.group && <Group color={group} onClick={parent.ungroup(photo)} />}
        <img alt={photo.name} src={utils.getSrc(photo.src, 200, 200, true)} />
      </div>,
    ));
  }
}

export default DragSource(PHOTO, photoSource, collectDrag)(
  DropTarget(PHOTO, photoDrop, collectDrop)(Photo),
);
