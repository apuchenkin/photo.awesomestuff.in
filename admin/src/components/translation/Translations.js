import React from 'react';
import classNames from 'classnames';
import { Link } from 'react-router-dom';

import Translation from './Translation';

const langs = ['ru', 'en'];

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

    this.remove = this.remove.bind(this);
    this.update = this.update.bind(this);
  }

  remove(translation) {
    // eslint-disable-next-line no-alert
    if (window.confirm(`Delete translation ${translation.value}?`)) {
      this.props.remove(translation);
    }
  }

  update(translation, data) {
    return translation.id
      ? this.props.update(translation, data)
      : this.props.create(Object.assign(translation, data))
    ;
  }

  render() {
    const { backUrl, title, translations, children, fields, entity } = this.props;

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
          { children }
          <div className="translations">
            <table>
              <tbody>
                {fields.map(field => (langs.map((language) => {
                  const translation = translations.find(t =>
                    t.field === field &&
                    t.refType === entity &&
                    t.language === language,
                  ) || {
                    field,
                    language,
                    refType: entity,
                  };

                  return (
                    <Translation
                      key={`${field}-${language}`}
                      update={this.update}
                      remove={this.remove}
                      translation={translation}
                    />
                  );
                })))}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    );
  }
}

export default Translations;
