import React from 'react';
import classNames from 'classnames';
import { DropTarget } from 'react-dnd';
import { NavLink } from 'react-router-dom';

// /**
//  * Specifies which props to inject into your component.
//  */


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

const Category = (props) => {
  const category = props.data;
  const dropTarget = props.dropTarget;
  // const highlighted = props.highlighted;
  const hovered = props.hovered;

  return dropTarget(
    <div className={classNames({
      category: true,
      isHidden: category.hidden,
      'category--hovered': hovered,
    })}
    >
      <NavLink to={`/category/${category.name}`} activeClassName="active">{category.name}</NavLink>
    </div>,
  );
};

export default DropTarget(PHOTO, categoryDrop, collectDrop)(Category);
