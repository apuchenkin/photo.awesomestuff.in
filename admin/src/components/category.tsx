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
  ConnectDropTarget,
  ConnectDragSource,
} from 'react-dnd';
import { NavLink, Link } from 'react-router-dom';
import { CategoryContext, ServiceContext } from '@app/context';
import { UpdateCategory, DeleteCategory } from '@app/context/category';

const PHOTO = 'photo';
const CATEGORY = 'category';

interface ExternalProps {
  category: Category;
}

interface Props extends ExternalProps {
  updateCategory: UpdateCategory;
  deleteCategory: DeleteCategory;
  linkPhotos: (category: Category, photos: Photo[]) => void;
  dragSource: ConnectDragSource;
  dropTarget: ConnectDropTarget;
  hovered: boolean;
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
  drop: ({ category, updateCategory, linkPhotos }, monitor) => {
    switch (monitor.getItemType()) {
      case PHOTO:
        linkPhotos(category, [monitor.getItem()]);
        break;
      case CATEGORY:
        updateCategory({
          ...category,
          parent: monitor.getItem(),
        })
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

const Category: React.FunctionComponent<Props> = ({
  category,
  deleteCategory,
  updateCategory,
  dragSource,
  dropTarget,
  hovered,
}) => {
  const remove = () => {
    if (window.confirm(`Delete category ${category.name}?`)) {
      deleteCategory(category);
    }
  };

  const toggleVisibility = () => updateCategory({
    ...category,
    hidden: !category.hidden,
  });

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
  (cmp: React.ComponentType<any>) => (props: ExternalProps) => {
    const { categoryService } = React.useContext(ServiceContext);
    const {
      updateCategory,
      deleteCategory,
    } = React.useContext(CategoryContext);

    return React.createElement(cmp, {
      ...props,
      updateCategory,
      deleteCategory,
      linkPhotos: categoryService.linkPhotos,
    });
  },
  DragSource(CATEGORY, categorySource, collectDrag),
  DropTarget([CATEGORY, PHOTO], categoryDrop, collectDrop),
)(Category);