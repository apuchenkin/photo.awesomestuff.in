import React from 'react';

class Translations extends React.PureComponent {
  constructor(props) {
    super(props);
    console.log(props.translation);

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
      <tr className={translation.value ? '' : 'error'}>
        <td width="30">{translation.language}</td>
        <td width="50">{translation.field}</td>
        <td>{edit
          ? <input
            ref={(input) => { this.input = input; }}
            type="text"
            name="value"
            defaultValue={translation.value}
          />
          : translation.value}
        </td>
        <td width="130">
          {edit
          ? ([
            <button key="submit" onClick={this.submit}>
              Save
            </button>,
            <button key="cancel" onClick={this.cancel}>
              Cancel
            </button>,
          ])
          : ([
            <button key="edit" className="material-icons" onClick={this.toggleEdit}>
              mode_edit
            </button>,
            translation.id && (
              <button key="remove" className="material-icons" onClick={() => remove(translation)}>
                clear
              </button>
            ),
          ])}
        </td>
      </tr>
    );
  }
}

export default Translations;
