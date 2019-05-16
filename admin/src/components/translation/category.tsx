import * as React from 'react';
import Translations from './translations';

const FIELDS = ['title'];

interface Props {
  category: Category;
}

const CategoryTranslations: React.FunctionComponent<Props> = ({ category }) => (
  <Translations
    fields={FIELDS}
    translations={category.translations}
    title={category.name}
  />
);

export default CategoryTranslations;
