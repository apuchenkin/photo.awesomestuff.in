import * as React from 'react';
import classNames from 'classnames';
import {
  DragSource,
  DropTarget,
  DragSourceSpec,
  ConnectDragSource,
  ConnectDropTarget,
  DropTargetSpec,
  DropTargetCollector,
  DragSourceCollector,
} from 'react-dnd';
import { Link } from 'react-router-dom';
import { compose } from 'ramda';
import { getThumb } from '@app/service/photo';
import { PhotoContext } from '@app/context';
import { __RouterContext } from 'react-router';
import { GroupPhotos } from '@app/context/photo';

const PHOTO = 'photo';

interface ExternalProps {
  photo: Photo;
  featured: boolean;
}

interface Props extends ExternalProps {
  group: GroupPhotos;
  isDragging: boolean;
  dragSource: ConnectDragSource;
  dropTarget: ConnectDropTarget;
  highlighted: boolean;
  hovered: boolean;
}

const photoSource: DragSourceSpec<Props, Photo> = {
  beginDrag: ({ photo }) => photo,
}

const photoDrop: DropTargetSpec<Props> = {
  drop({ group, photo }, monitor) {
    group([monitor.getItem(), photo]);
  },
  canDrop({ photo }, monitor) {
    return photo.id !== monitor.getItem().id;
  },
};

const collectDrop: DropTargetCollector<{}, {}> = (connect, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: connect.dropTarget(),
});

const collectDrag: DragSourceCollector<{}, {}> = (connect, monitor) => ({
  dragSource: connect.dragSource(),
  isDragging: monitor.isDragging(),
});

const translateColor = (photo: Photo) => {
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

const Photo: React.FunctionComponent<Props> = ({
  photo,
  featured,
  isDragging,
  dragSource,
  dropTarget,
  highlighted,
  hovered,
}) => {
  const { match, history } = React.useContext(__RouterContext);
  const { isSelected, select, ungroup, getGroup, hasParent } = React.useContext(PhotoContext);

  const Group = () => (
    <div
      role="button"
      className="group"
      style={{ background: getGroup(photo) }}
      onClick={() => ungroup(photo)}
    />
  );

  return dragSource(dropTarget(
    <div
      className={classNames({
        photo: true,
        'photo--highlighted': highlighted,
        'photo--hovered': hovered,
        dragging: isDragging,
        selected: isSelected(photo),
        hasParent: hasParent(photo),
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
      {hasParent(photo) && <div className="parent" />}
      {photo.group && <Group />}
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
  (cmp: React.ComponentType<any>) => (props: ExternalProps) => {
    const { group } = React.useContext(PhotoContext);

    return React.createElement(cmp, {
      ...props,
      group,
    });
  },
  DragSource(PHOTO, photoSource, collectDrag),
  DropTarget(PHOTO, photoDrop, collectDrop),
)(Photo);
