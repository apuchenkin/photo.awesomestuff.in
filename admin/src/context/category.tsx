import * as React from 'react';
import { Context as ServiceContext } from './service';
import { values, indexBy, prop, dissoc } from 'ramda';

export type GetCategories = () => Category[];
export type GetCategory = (name: string) => Category | undefined;
export type CreateCategory = (category: Partial<Category>) => void;
export type UpdateCategory = (category: Category) => void;
export type DeleteCategory = (category: Category) => void;

interface Props {
  getCategories: GetCategories;
  getCategory: GetCategory;
  createCategory: CreateCategory;
  updateCategory: UpdateCategory;
  deleteCategory: DeleteCategory;
}

// @ts-ignore
export const Context = React.createContext<Props>();

const CategoryProvider: React.FunctionComponent = ({ children }) => {
  const { categoryService } = React.useContext(ServiceContext);
  const [categories, setCategories] = React.useState<Record<string, Category>>({});

  React.useEffect(() => {
    categoryService.fetchCategories()
      .then(indexBy(prop<string, string>('name')))
      .then(setCategories);
  }, [])

  return (
    <Context.Provider
      value={{
        getCategories: () => values(categories),
        getCategory: (name: string) => categories[name],
        createCategory: (category: Partial<Category>) => {
          const name = category.name;
          // @ts-ignore
          const category$: Category = { name };

          setCategories({
            ...categories,
            [category$.name]: category$,
          })
        },
        updateCategory: (category: Category) => {
          setCategories({
            ...categories,
            [category.name]: category,
          })
        },
        deleteCategory: (category: Category) => {
          setCategories(dissoc(category.name, categories));
        }
      }}
    >
      {children}
    </Context.Provider>
  );
}

export default CategoryProvider;
