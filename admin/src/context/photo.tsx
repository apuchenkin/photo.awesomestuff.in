import * as React from 'react';
import { Context as ServiceContext } from './service';
import { values, indexBy, prop, dissoc } from 'ramda';

export type GetPhotos = () => Photo[];
export type GetGroups = () => any[];
export type GetPhoto = (id: number) => Photo | undefined;
// export type UpdateCategory = (category: Category) => void;
// export type DeleteCategory = (category: Category) => void;

interface ContextProps {
  getPhotos: GetPhotos;
  getPhoto: GetPhoto;
  getGroups: GetGroups;
  // updateCategory: UpdateCategory;
  // deleteCategory: DeleteCategory;
}

// @ts-ignore
export const Context = React.createContext<ContextProps>();

interface Props {
  category: Category;
}

const PhotoProvider: React.FunctionComponent<Props> = ({ category, children }) => {
  const { categoryService } = React.useContext(ServiceContext);
  const [ photos, setPhotos ] = React.useState<Record<string, Category>>({});
  const [ selection, setSelection ] = React.useState([]);

  React.useEffect(() => {
    categoryService.fetchPhotos(category)
      .then(indexBy(prop<string, string>('id')))
      .then(setPhotos);
  }, [])

  const cleanSelection = () => {
    setSelection([]);
  }

  const isSelected = (photo: Photo) => {
    return selection
      && selection.length
      && selection.find(p => p.id === photo.id);
  }

  const select = (photo: Photo, shift: boolean) => {
    let selection$;
    if (shift) {
      selection$ = isSelected(photo)
        ? selection.filter(p => p.id !== photo.id)
        : [...selection, photo];
    } else {
      selection$ = [photo];
    }

    setSelection(selection$);
  }

  const getSelectionCount = () => selection.length;

  return (
    <Context.Provider
      value={{
        getPhotos: () => values(photos),
        getPhoto: (id: number) => photos[id],
        getGroups: (): any[] => [],
        cleanSelection,
        isSelected,
        select,
        getSelectionCount,
      }}
    >
      {children}
    </Context.Provider>
  );
}

export default PhotoProvider;
