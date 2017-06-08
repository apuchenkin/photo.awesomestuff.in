import React from 'react';
import classNames from 'classnames';
import Category from './Category';

const categories = (data, admin) => data.map(category => (
  <li className="item" key={category.id} >
    <Category data={category} admin={admin} />
  </li>
));

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

export default class Categories extends React.Component {
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
    const { data, admin } = this.props;
    const { add } = this.state;

    return (
      <nav className="categories">
        <h2>Categories <AddButton handler={this.toggleCreate} add={add} /></h2>
        { add && addForm(this) }
        <ul>{categories(data, admin)}</ul>
      </nav>
    );
  }
}
