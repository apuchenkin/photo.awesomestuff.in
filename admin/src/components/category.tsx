import * as React from 'react';
import { compose } from 'ramda';
import classNames from 'classnames';
import {
  DragSource,
  DropTarget,
  DragSourceSpec,
  DragSourceCollector,
  DropTargetCollector,
  DropTargetSpec,
} from 'react-dnd';
import { NavLink, Link } from 'react-router-dom';
import { CategoryContext } from '@app/context';

const PHOTO = 'photo';
const CATEGORY = 'category';

interface Props {
  category: Category;
}

const categorySource: DragSourceSpec<Props, Category> = {
  beginDrag: ({ category }) => category,
};

const collectDrag: DragSourceCollector<{}, {}> = (connect, monitor) => ({
  dragSource: connect.dragSource(),
  isDragging: monitor.isDragging(),
});

const collectDrop: DropTargetCollector<{}, {}> = (connect, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: connect.dropTarget(),
});

const categoryDrop: DropTargetSpec<Props> = {
  drop: ({ category, setParent, linkPhotos }, monitor) => {
    switch (monitor.getItemType()) {
      case PHOTO:
        linkPhotos(category, [monitor.getItem()]);
        break;
      case CATEGORY:
        setParent(category, monitor.getItem())
        break;
    }
  },
  canDrop: ({ category }, monitor) => {
    switch (monitor.getItemType()) {
      case PHOTO:
        return true;
      case CATEGORY:
        return !category.parent && category.id !== monitor.getItem().id;
    }
  },
};

const translateColor = (category: Category) => {
  if (!(category.translations && category.translations.length)) {
    return 'red';
  }

  if (category.translations.find(translation => translation.language === 'ru')
   && category.translations.find(translation => translation.language === 'en')
  ) {
    return 'green';
  }

  return 'yellow';
};

const Category = ({
  category,
  dragSource,
  dropTarget,
  hovered,
}) => {
  const remove = () => {
    if (window.confirm(`Delete category ${category.name}?`)) {
      deleteCategory(category);
    }
  };

  const toggleVisibility = () => updateCategory(category, { hidden: !category.hidden });

  return dragSource(dropTarget(
    <div className={classNames('category', {
      isHidden: category.hidden,
      'category--hovered': hovered,
    })}
    >
      <NavLink to={`/category/${category.name}/photo`} activeClassName="active">{category.name}</NavLink>
      <span className="tools">
        <Link to={`/category/${category.name}/translation`} >
          <button className="material-icons" style={{ color: translateColor(category) }}>
            translate
          </button>
        </Link>
        <button
          onClick={toggleVisibility}
          className="material-icons"
        >
          {category.hidden ? 'visibility' : 'visibility_off'}
        </button>
        <button
          onClick={remove}
          className="material-icons"
        >
          delete_forever
        </button>
      </span>
    </div>,
  ));
};

export default compose(
  (cmp: React.Component) => (props: Props) => {
    const categories = React.useContext(CategoryContext);
    console.log(cmp, categories);

    return React.createElement(cmp, props);
  },
  DragSource(CATEGORY, categorySource, collectDrag),
  DropTarget([CATEGORY, PHOTO], categoryDrop, collectDrop),
)(Category);