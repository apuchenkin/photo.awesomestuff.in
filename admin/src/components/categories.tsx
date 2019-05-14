import * as React from 'react';
import classNames from 'classnames';
import { DropTarget } from 'react-dnd';
import RootCategory from './RootCategory';

const addForm = ctx => (
  <form className="create" onSubmit={ctx.submit}>
    <input name="category" />
    <input type="submit" value="Submit" />
    <button onClick={ctx.cancel}>Cancel</button>
  </form>
);

const AddButton = ({ handler, add }) => (
  <i
    role="button"
    tabIndex="0"
    onClick={handler}
    className={classNames('material-icons', { active: add })}
  >
    add
  </i>
);

const categoryDrop = {
  drop({ updateCategory }, monitor) {
    updateCategory(monitor.getItem(), {
      parentId: null,
    });
  },
  canDrop() {
    return true;
  },
};

const collectDrop = ({ dropTarget }, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: dropTarget(),
});

// submit(e) {
//   e.preventDefault();
//   const data = new FormData(e.target);

//   this.props.categoryCreate({
//     name: data.get('category'),
//   });
//   this.cancel();
// }

const Categories = ({ categories, dropTarget }) => {
  const rootCategories = categories
    .filter(c => !c.parentId)
    .map(category => ({
      ...category,
      childs: categories.filter(c => c.parentId === category.id),
      collapsed: true,
    }))
    .map(category => <RootCategory category={category} key={category.id} />);

  return (
    <nav className="categories">
      { dropTarget(<h2>Categories <AddButton handler={this.toggleCreate} add={add} /></h2>) }
      { add && addForm(this) }
      <ul>{rootCategories}</ul>
    </nav>
  );
}

export default DropTarget('category', categoryDrop, collectDrop)(Categories);
