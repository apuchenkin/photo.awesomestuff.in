import * as React from 'react';
import { Context as ServiceContext } from './service';

// @ts-ignore
export const Context = React.createContext<any[]>();

const CategoryProvider: React.FunctionComponent = ({ children }) => {
  const { categoryService } = React.useContext(ServiceContext);
  const [categories, setCategories] = React.useState([]);

  React.useEffect(() => {
    categoryService.fetchCategories().then(setCategories);
  }, [])

  return (
    <Context.Provider
      value={categories}
    >
      {children}
    </Context.Provider>
  );
}

export default CategoryProvider;
