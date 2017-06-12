import React from 'react';
import classNames from 'classnames';
import { DragSource, DropTarget } from 'react-dnd';
import { NavLink, Link } from 'react-router-dom';

const PHOTO = 'photo';
export const CATEGORY = Symbol('category');

const categorySource = {
  beginDrag: ({ category }) => category,
};

const collectDrag = (connect, monitor) => ({
  // Call this function inside render()
  // to let React DnD handle the drag events:
  dragSource: connect.dragSource(),
  // You can ask the monitor about the current drag state:
  isDragging: monitor.isDragging(),
});

const collectDrop = (connect, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: connect.dropTarget(),
});

const setParent = (admin, categoryDroped, categoryDraged) => {
  admin.categoryService.update(categoryDraged.name, {
    parentId: categoryDroped.id,
  }).then(admin.fetchCategories);
};

const categoryDrop = {
  drop({ admin, category }, monitor) {
    return {
      [PHOTO]: () => admin.addToCategory(category, monitor.getItem()),
      [CATEGORY]: () => setParent(admin, category, monitor.getItem()),
    }[monitor.getItemType()]();
  },
  canDrop() {
    return true;
  },
};

const deleteCategory = (admin, category) => () => {
  // eslint-disable-next-line no-alert
  if (window.confirm(`Delete category ${category.name}?`)) {
    admin.categoryService.delete(category.name).then(() => {
      admin.fetchCategories();
    });
  }
};

const toggleVisibility = (admin, category) => () => {
  admin.categoryService.update(category.name, { hidden: !category.hidden }).then(() => {
    admin.fetchCategories();
  });
};

const Category = (props) => {
  const { category, admin, dragSource, dropTarget, hovered } = props;

  return dragSource(dropTarget(
    <div className={classNames('category', {
      isHidden: category.hidden,
      'category--hovered': hovered,
    })}
    >
      <NavLink to={`/category/${category.name}`} activeClassName="active">{category.name}</NavLink>
      <span className="tools">
        <Link to={`/category/${category.name}/translation`} >
          <button className="material-icons">
            translate
          </button>
        </Link>
        <button
          onClick={toggleVisibility(admin, category)}
          className="material-icons"
        >
          {category.hidden ? 'visibility' : 'visibility_off'}
        </button>
        <button
          onClick={deleteCategory(admin, category)}
          className="material-icons"
        >
          clear
        </button>
      </span>
    </div>,
  ));
};

export default DragSource(CATEGORY, categorySource, collectDrag)(
  DropTarget([CATEGORY, PHOTO], categoryDrop, collectDrop)(Category),
);
