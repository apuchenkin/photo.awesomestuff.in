import * as R from 'ramda';
import { Category } from "@app/entity";

export interface CategoryDTO {
  name: string;
  title: string;
  description?: string;
  date?: Date;
  featured?: any;
  children: CategoryDTO[];
}

export const unnest = (categories: Category[]): Category[] => {
  const children = R.groupBy(R.pathOr('root', ['parent', 'name']), categories);

  return children.root.map(category => ({
    ...category,
    children: children[category.name] || []
  }))
}

export const categoryDTO = (category: Category): CategoryDTO => {
  const translations = R.map(
    R.prop('value'),
    R.indexBy(R.prop('field'), category.translations),
  );
  const children = category.children || [];

  return {
    name: category.name,
    date: category.date,
    title: translations.title,
    description: translations.description,
    children: children.map(categoryDTO),
  }
}