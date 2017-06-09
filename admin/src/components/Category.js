import React from 'react';
import classNames from 'classnames';
import { DropTarget } from 'react-dnd';
import { NavLink, Link } from 'react-router-dom';

const PHOTO = 'photo';

const collectDrop = (connect, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: connect.dropTarget(),
});

const categoryDrop = {
  drop({ admin, data }, monitor) {
    admin.addToCategory(data, monitor.getItem());
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
  const category = props.data;
  const { admin, dropTarget, hovered } = props;

  return dropTarget(
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
  );
};

export default DropTarget(PHOTO, categoryDrop, collectDrop)(Category);
