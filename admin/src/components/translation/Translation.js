import React from 'react';

class Translations extends React.PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      edit: false,
    };

    this.toggleEdit = this.toggleEdit.bind(this);
    this.cancel = this.cancel.bind(this);
    this.submit = this.submit.bind(this);
  }

  toggleEdit() {
    this.setState((state => ({ edit: !state.edit })));
  }

  cancel() {
    this.setState({ edit: false });
  }

  submit() {
    const { translation } = this.props;

    this.props.update(translation, {
      value: this.input.value,
    });

    this.setState({
      edit: false,
    });
  }

  render() {
    const { translation, remove } = this.props;
    const { edit } = this.state;

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
          <button className="material-icons" onClick={() => remove(translation)}>
            clear
          </button>
        </td>
      </tr>
    );
  }
}

export default Translations;
