import React from 'react';
import classNames from 'classnames';
import { DropTarget } from 'react-dnd';
import { connect } from 'react-redux';
import RootCategory from './RootCategory';
import { CATEGORY } from './Category';

import {
  create as categoryCreate,
  update as categoryUpdate,
} from '../store/category/actions';

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

class Categories extends React.PureComponent {
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

    this.props.categoryCreate({
      name: data.get('category'),
    });
    this.cancel();
  }

  render() {
    const { categories, dropTarget } = this.props;
    const { add } = this.state;

    const rootCategories = categories
      .filter(c => !c.parentId)
      .map(category => Object.assign({}, category, {
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
}

export default connect(
  ({ category: { categories } }) => ({
    categories,
  }),
  dispatch => ({
    categoryCreate: data => dispatch(categoryCreate(data)),
    updateCategory: (category, data) => dispatch(categoryUpdate(category, data)),
  }),
)(DropTarget(CATEGORY, categoryDrop, collectDrop)(Categories));
