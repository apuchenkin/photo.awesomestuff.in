import * as React from 'react';
import { TranslationContext } from '@app/context';

interface Props {
  translation: Translation | Partial<Translation>;
}

const Translations: React.FunctionComponent<Props> = ({ translation }) => {
  const inputRef = React.useRef(null);
  const [edit, setEdit] = React.useState(false);
  const {
    createTranslation,
    updateTranslation,
    deleteTranslation,
  } = React.useContext(TranslationContext);

  const remove = (translation: Translation) => {
    if (window.confirm(`Delete translation ${translation.value}?`)) {
      deleteTranslation(translation);
    }
  }

  const toggleEdit = () => setEdit(!edit);
  const cancel = () => setEdit(false);
  const submit = () => {
    const translation$ = {
      ...translation,
      value: inputRef.current.value,
    };

    translation$.id
      ? updateTranslation(translation$)
      : createTranslation(translation$)
    ;

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
            <button key="remove" className="material-icons" onClick={() => remove(translation as Translation)}>
              clear
            </button>
          ),
        ])}
      </td>
    </tr>
  )
}

export default Translations;
