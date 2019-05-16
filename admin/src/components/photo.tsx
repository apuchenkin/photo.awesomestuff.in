import * as React from 'react';
import classNames from 'classnames';
import { DragSource, DropTarget } from 'react-dnd';
import { Link } from 'react-router-dom';
import { compose } from 'ramda';
import { getThumb } from '@app/service/photo';
import { PhotoContext } from '@app/context';
import { __RouterContext } from 'react-router';

const PHOTO = 'photo';

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

const Photo = ({
  photo,
  group,
  featured,
  isDragging,
  dragSource,
  dropTarget,
  highlighted,
  hovered,
}) => {
  const { match, history } = React.useContext(__RouterContext);
  const { isSelected, select, ungroup } = React.useContext(PhotoContext);

  return dragSource(dropTarget(
    <div
      className={classNames({
        photo: true,
        'photo--highlighted': highlighted,
        'photo--hovered': hovered,
        dragging: isDragging,
        selected: isSelected(photo),
        hasParent: photo.hasParent,
        isHidden: photo.hidden,
      })}
      onClick={e => select(photo, e.ctrlKey)}
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
      {photo.group && <Group color={group} onClick={() => ungroup(photo)} />}
      <div
        className="img"
        style={{
          width: 150,
          height: 150,
          backgroundColor: 'grey',
          backgroundImage: `url(${getThumb(200, photo.src)})`,
          backgroundPosition: 'center center',
          backgroundSize: 'cover',
          backgroundRepeat: 'no-repeat',
        }}
      />
    </div>,
  ));
}

export default compose(
  DragSource(PHOTO, photoSource, collectDrag),
  DropTarget(PHOTO, photoDrop, collectDrop),
)(Photo);
