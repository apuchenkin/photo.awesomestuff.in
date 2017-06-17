import React from 'react';

class Translations extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      edit: false,
      translation: props.translation,
    };

    this.toggleEdit = this.toggleEdit.bind(this);
    this.cancel = this.cancel.bind(this);
    this.submit = this.submit.bind(this);
  }

  toggleEdit() {
    this.setState((state => ({ edit: !state.add })));
  }

  cancel() {
    this.setState({ edit: false });
  }

  submit() {
    const { parent } = this.props;
    const { translation } = this.state;

    parent.updateTranslation(translation, {
      value: this.input.value,
    }).then((t) => {
      this.setState({
        edit: false,
        translation: t, //Object.assign(translation, { value })
      });
    });
  }

  render() {
    const { parent } = this.props;
    const { edit, translation } = this.state;

    return (
      <tr>
        <td>{translation.language}</td>
        <td>{translation.field}</td>
        <td>{edit
          ? <input
            ref={(input) => { this.input = input; }}
            type="text"
            name="value"
            defaultValue={translation.value}
          />
          : translation.value}
        </td>
        <td>
          {edit
          ? ([
            <button key="submit" onClick={this.submit}>
              Save
            </button>,
            <button key="cancel" onClick={this.cancel}>
              Cancel
            </button>,
          ])
          : (
            <button className="material-icons" onClick={this.toggleEdit}>
              mode_edit
            </button>
          )}
          <button className="material-icons" onClick={parent.delete(translation)}>
            clear
          </button>
        </td>
      </tr>
    );
  }
}

export default Translations;
