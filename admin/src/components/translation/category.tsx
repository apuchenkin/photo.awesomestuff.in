import * as React from 'react';
import Translations from './translations';
import { CategoryContext, TranslationProvider } from '@app/context';
import { UpdateTranslations } from '@app/context/translation';

const FIELDS = ['title'];

interface Props {
  category: Category;
}

const CategoryTranslations: React.FunctionComponent<Props> = ({ category }) => {
  const { updateCategory } = React.useContext(CategoryContext);

  const update: UpdateTranslations = translations => updateCategory({
    ...category,
    translations,
  });

  return (
    <TranslationProvider translations={category.translations} update={update}>
      <Translations
        fields={FIELDS}
        translations={category.translations}
        title={category.name}
      />
    </TranslationProvider>
  )
};

export default CategoryTranslations;
