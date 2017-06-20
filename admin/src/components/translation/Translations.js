import React from 'react';
import classNames from 'classnames';
import { Link } from 'react-router-dom';

import Translation from './Translation';

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

class Translations extends React.PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      add: false,
    };

    this.toggleCreate = this.toggleCreate.bind(this);
    this.cancel = this.cancel.bind(this);
    this.submit = this.submit.bind(this);
    this.remove = this.remove.bind(this);
    this.update = this.update.bind(this);
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

    this.props.create({
      language: data.get('language'),
      value: data.get('value'),
      field: data.get('field'),
    });

    this.cancel();
  }

  remove(translation) {
    // eslint-disable-next-line no-alert
    if (window.confirm(`Delete translation ${translation.value}?`)) {
      this.props.remove(translation);
    }
  }

  update(translation, data) {
    return this.props.update(translation, data);
  }

  render() {
    const { backUrl, title, translations, children } = this.props;
    const { add } = this.state;

    const translationsCmp = translations.map(translation => (
      <Translation
        key={translation.id}
        update={this.update}
        remove={this.remove}
        translation={translation}
      />
    ));

    return (
      <div className="translation">
        <div className="toolbox">
          Translations - {title}
          <div className="tools">
            <AddButton handler={this.toggleCreate} />
            <Link to={backUrl} >
              <button className="material-icons">
                clear
              </button>
            </Link>
          </div>
        </div>
        <div className="content">
          { add && addForm(this)}
          { children }
          <table>
            <tbody>
              { translationsCmp }
            </tbody>
          </table>
        </div>
      </div>
    );
  }
}

export default Translations;
