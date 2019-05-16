import * as React from 'react';

interface Props {
  translation: Translation;
}

const Translations: React.FunctionComponent<Props> = ({ translation }) => {
  const inputRef = React.useRef(null);
  const [edit, setEdit] = React.useState(false);

  // TODO: up!
  const update = (translation: Translation) => {};
  const remove = (translation: Translation) => {};

  const toggleEdit = () => setEdit(!edit);
  const cancel = () => setEdit(false);
  const submit = () => {
    // update(translation, {
    //   value: inputRef.current.value,
    // });

    cancel();
  }

  return (
    <tr className={translation.value ? '' : 'error'}>
      <td>{translation.language}</td>
      <td>{translation.field}</td>
      <td>{
        edit
          ? (<input
            ref={inputRef}
            type="text"
            name="value"
            defaultValue={translation.value}
          />)
          : translation.value
        }
      </td>
      <td>
        {edit
        ? ([
          <button key="submit" onClick={submit}>
            Save
          </button>,
          <button key="cancel" onClick={cancel}>
            Cancel
          </button>,
        ])
        : ([
          <button key="edit" className="material-icons" onClick={toggleEdit}>
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
  )
}

export default Translations;
