import * as React from 'react';
import { compose } from 'ramda';
import classNames from 'classnames';
import {
  DropTarget,
  DropTargetCollector,
  ConnectDropTarget,
  DropTargetSpec,
} from 'react-dnd';

import RootCategory from './rootCategory';
import { CategoryContext } from '@app/context';
import { UpdateCategory, CreateCategory } from '@app/context/category';

const categoryDrop: DropTargetSpec<Props> = {
  drop: ({ updateCategory }, monitor) => {
    updateCategory({
      ...monitor.getItem(),
      parent: null,
    });
  },
  canDrop: () => true,
};

const collectDrop: DropTargetCollector<{}, {}> = ({ dropTarget }, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: dropTarget(),
});

interface ExternalProps {
  categories: Category[];
}

interface Props extends ExternalProps {
  dropTarget: ConnectDropTarget;
  updateCategory: UpdateCategory;
  createCategory: CreateCategory;
}

const Categories: React.FunctionComponent<Props> = ({ categories, createCategory, dropTarget }) => {
  const [add, setAdd] = React.useState(false);
  const inputRef = React.useRef(null);
  const rootCategories = categories
    .filter(c => !(c.parent && c.parent.id))
    .map(category => ({
      ...category,
      childs: categories.filter(c => (c.parent && c.parent.id) === category.id),
      collapsed: true,
    }))
    .map(category => (<RootCategory category={category} key={category.id} />));

  const cancel = () => setAdd(false);

  const submit: React.FormEventHandler = (event) => {
    event.preventDefault();
    if (inputRef.current) {
      createCategory({ name: inputRef.current.value });
    }

    cancel();
  }

  const AddForm = () => (
    <form className="create" onSubmit={submit}>
      <input name="category" ref={inputRef} />
      <input type="submit" value="Submit" />
      <button onClick={cancel}>Cancel</button>
    </form>
  );

  const AddButton = () => (
    <i
      role="button"
      tabIndex={0}
      onClick={() => setAdd(true)}
      className={classNames('material-icons', { active: add })}
    >
      add
    </i>
  );

  return (
    <nav className="categories">
      {dropTarget(<h2>Categories <AddButton /></h2>)}
      {add && <AddForm />}
      <ul>{rootCategories}</ul>
    </nav>
  );
}

export default compose(
  (cmp: React.ComponentType<any>) => (props: ExternalProps) => {
    const { createCategory, updateCategory } = React.useContext(CategoryContext);

    return React.createElement(cmp, {
      ...props,
      createCategory,
      updateCategory
    });
  },
  DropTarget('category', categoryDrop, collectDrop),
)(Categories);
