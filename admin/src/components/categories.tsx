import * as React from 'react';
import { compose } from 'ramda';
import classNames from 'classnames';
import {
  DropTarget,
  DropTargetCollector,
  ConnectDropTarget,
  DropTargetSpec,
} from 'react-dnd';
import RootCategory from './RootCategory';
import { CategoryContext } from '@app/context';

const categoryDrop: DropTargetSpec<Props> = {
  drop: ({ updateCategory, ...props }, monitor) => {
    debugger;
    updateCategory(monitor.getItem(), {
      parentId: null,
    });
  },
  canDrop: () => true,
};

const collectDrop: DropTargetCollector<{}, {}> = ({ dropTarget }, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: dropTarget(),
});

interface Props {
  categories: Category[];
  dropTarget: ConnectDropTarget;
}

const Categories: React.FunctionComponent<Props> = ({ categories, dropTarget }) => {
  // const inputEl = React.useRef(null);
  const rootCategories = categories
    .filter(c => !(c.parent && c.parent.id))
    .map(category => ({
      ...category,
      childs: categories.filter(c => (c.parent && c.parent.id) === category.id),
      collapsed: true,
    }))
    .map(category => (<RootCategory category={category} key={category.id} />));


  const cancel = () => {}
  const submit: React.FormEventHandler = (e) => {
    e.preventDefault();
    // const data = new FormData(e.currentTarget);

    // this.props.categoryCreate({
    //   name: data.get('category'),
    // });
    cancel();
  }

  const add = false;

  const AddForm = () => (
    <form className="create" onSubmit={submit}>
      <input name="category" />
      <input type="submit" value="Submit" />
      <button onClick={cancel}>Cancel</button>
    </form>
  );

  const AddButton = () => (
    <i
      role="button"
      tabIndex={0}
      // onClick={handler}
      className={classNames('material-icons', { active: add })}
    >
      add
    </i>
  );

  return (
    <nav className="categories">
      { dropTarget(<h2>Categories <AddButton /></h2>) }
      { add && <AddForm /> }
      <ul>{rootCategories}</ul>
    </nav>
  );
}

export default compose(
  // cmp => {

  //   const category = React.useContext(CategoryContext);
  //   console.log(category);
  //   debugger;

  //   return cmp;
  // },
  DropTarget('category', categoryDrop, collectDrop),
)(Categories);
