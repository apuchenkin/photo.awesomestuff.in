import * as React from 'react';

export type UpdateTranslations = (translations: Partial<Translation>[]) => void;
export type CreateTranslation = (translation: Partial<Translation>) => void;
export type UpdateTranslation = (category: Translation) => void;
export type DeleteTranslation = (category: Translation) => void;

interface ContextProps {
  createTranslation: CreateTranslation;
  updateTranslation: UpdateTranslation;
  deleteTranslation: DeleteTranslation;
}

// @ts-ignore
export const Context = React.createContext<ContextProps>();

interface Props {
  translations: Translation[];
  update: UpdateTranslations;
}

const TranslationProvider: React.FunctionComponent<Props> = ({ translations, update, children }) => {
  const createTranslation: CreateTranslation = (translation) => {
    update([...translations, translation]);
  };
  const updateTranslation: UpdateTranslation = (translation) => {
    update([
      ...translations.filter(translation$ => translation$.id !== translation.id),
      translation
    ]);
  };
  const deleteTranslation: DeleteTranslation = (translation) => {
    update(translations.filter(translation$ => translation$.id !== translation.id));
  };

  return (
    <Context.Provider
      value={{
        createTranslation,
        updateTranslation,
        deleteTranslation,
      }}
    >
      {children}
    </Context.Provider>
  );
}

export default TranslationProvider;
