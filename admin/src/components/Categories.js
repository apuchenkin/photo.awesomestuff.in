import React from 'react';
import classNames from 'classnames';
import { DropTarget } from 'react-dnd';
import RootCategory from './RootCategory';
import { CATEGORY } from './Category';

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

const removeParent = (admin, category) => {
  admin.categoryService.update(category.name, {
    parentId: null,
  }).then(admin.fetchCategories);
};

const categoryDrop = {
  drop({ admin }, monitor) {
    removeParent(admin, monitor.getItem());
  },
  canDrop() {
    return true;
  },
};

const collectDrop = (connect, monitor) => ({
  highlighted: monitor.canDrop(),
  hovered: monitor.isOver() && monitor.canDrop(),
  dropTarget: connect.dropTarget(),
});

class Categories extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      add: false,
    };

    this.toggleCreate = this.toggleCreate.bind(this);
    this.cancel = this.cancel.bind(this);
    this.submit = this.submit.bind(this);
  }

  toggleCreate() {
    this.setState((state => ({ add: !state.add })));
  }

  cancel() {
    this.setState({ add: false });
  }

  submit(e) {
    e.preventDefault();
    const data = new FormData(e.target);
    const { admin } = this.props;

    admin.categoryService.create({
      name: data.get('category'),
    }).then(() => {
      admin.fetchCategories();
      this.setState({ add: false });
    });
  }

  render() {
    const { categories, admin, dropTarget } = this.props;
    const { add } = this.state;
    const rootCategories = categories
      .filter(c => !c.parentId)
      .map(category => Object.assign(category, {
        childs: categories.filter(c => c.parentId === category.id),
        collapsed: true,
      }))
      .map(category => <RootCategory category={category} admin={admin} key={category.id} />);

    return (
      <nav className="categories">
        { dropTarget(<h2>Categories <AddButton handler={this.toggleCreate} add={add} /></h2>) }
        { add && addForm(this) }
        <ul>{rootCategories}</ul>
      </nav>
    );
  }
}

export default DropTarget(CATEGORY, categoryDrop, collectDrop)(Categories);
