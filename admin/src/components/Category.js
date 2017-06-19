import React from 'react';
import classNames from 'classnames';
import { DragSource, DropTarget } from 'react-dnd';
import { NavLink, Link } from 'react-router-dom';
import { connect } from 'react-redux';
import {
  update as categoryUpdate,
  remove as categoryDelete,
} from '../store/category/actions';

const PHOTO = 'photo';
export const CATEGORY = Symbol('category');

const categorySource = {
  beginDrag: ({ category }) => category,
};

const collectDrag = ({ dragSource }, monitor) => ({
  dragSource: dragSource(),
  isDragging: monitor.isDragging(),
});

const collectDrop = ({ dropTarget }, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: dropTarget(),
});

const setParent = (update, categoryDroped, categoryDraged) => {
  update(categoryDraged, {
    parentId: categoryDroped.id,
  });
};

const categoryDrop = {
  drop({ updateCategory, category, categoryService }, monitor) {
    return {
      [PHOTO]: () => categoryService.linkPhotos(category, monitor.getItem()),
      [CATEGORY]: () => setParent(updateCategory, category, monitor.getItem()),
    }[monitor.getItemType()]();
  },
  canDrop({ admin, category }, monitor) {
    return {
      [PHOTO]: true,
      [CATEGORY]: category.parentId === null && category.id !== monitor.getItem().id,
    }[monitor.getItemType()];
  },
};

const translateColor = (category) => {
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

const Category = (props) => {
  const {
    category,
    dragSource,
    dropTarget,
    hovered,
    deleteCategory,
    updateCategory,
  } = props;

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
          clear
        </button>
      </span>
    </div>,
  ));
};

export default connect(
  ({ runtime: { categoryService } }) => ({
    categoryService,
  }),
  dispatch => ({
    updateCategory: (category, data) => dispatch(categoryUpdate(category, data)),
    deleteCategory: category => dispatch(categoryDelete(category)),
  }),
)(
  DragSource(CATEGORY, categorySource, collectDrag)(
    DropTarget([CATEGORY, PHOTO], categoryDrop, collectDrop)(Category),
  ),
);
