import React from 'react';
import classNames from 'classnames';
import { Link } from 'react-router-dom';

const langs = ['ru', 'en'];

const addForm = ctx => (
  <form className="create" onSubmit={ctx.submit}>
    <select name="language">
      {langs.map(lang => <option key={lang} value={lang}>{lang}</option>)}
    </select>
    <input name="value" />
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

class Translation extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      add: false,
      translations: [],
    };

    this.toggleCreate = this.toggleCreate.bind(this);
    this.cancel = this.cancel.bind(this);
    this.submit = this.submit.bind(this);
  }

  componentWillMount() {
    const { categoryName, admin } = this.props;

    admin.categoryService.fetchTranslations(categoryName)
      .then((translations) => {
        this.setState({ translations });
      });
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
    const { admin, categoryName } = this.props;

    admin.categoryService.createTranslation(categoryName, {
      language: data.get('language'),
      value: data.get('value'),
      field: 'title',
    }).then((translation) => {
      this.setState((state => ({
        add: false,
        translations: [translation, ...state.translations],
      })));
    });
  }

  delete(translation) {
    const { admin, categoryName } = this.props;

    return () => {
      admin.categoryService.deleteTranslation(categoryName, translation.id).then(() => {
        this.setState(({ translations }) => ({
          translations: translations.filter(t => t !== translation),
        }));
      });
    };
  }

  render() {
    const { categoryName } = this.props;
    const { translations, add } = this.state;
    const translationRows = translations.map(translation => (
      <tr>
        <td>{translation.language}</td>
        <td>{translation.field}</td>
        <td>{translation.value}</td>
        <td>
          <button className="material-icons">
            mode_edit
          </button>
          <button className="material-icons" onClick={this.delete(translation)}>
            clear
          </button>
        </td>
      </tr>
    ));

    return (
      <div className="translation">
        <div className="toolbox">
          Translations
          <div className="tools">
            <AddButton handler={this.toggleCreate} />
            <Link to={`/category/${categoryName}`} >
              <button className="material-icons">
                clear
              </button>
            </Link>
          </div>
        </div>
        <div className="content">
          { add && addForm(this)}
          <table>{ translationRows }</table>
        </div>
      </div>
    );
  }
}

export default Translation;
