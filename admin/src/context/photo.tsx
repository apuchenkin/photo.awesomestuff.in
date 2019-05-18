import * as React from 'react';
import { Context as ServiceContext } from './service';
import { values, indexBy, prop, slice, range } from 'ramda';
import { CategoryContext } from '.';

export type GetTotal = () => number;
export type GetPhotos = (page?: number, limit?: number) => Photo[];
export type GetPhoto = (id: number) => Photo | undefined;
export type DeletePhotos = (photos: Photo[]) => void;
export type GroupPhotos = (photos: Photo[]) => void;

interface ContextProps {
  getTotal: GetTotal;
  getPhotos: GetPhotos;
  getPhoto: GetPhoto;
  deletePhotos: DeletePhotos;
  group: GroupPhotos;
}

// @ts-ignore
export const Context = React.createContext<ContextProps>();

interface Props {
  category: Category;
}

const getRandomColor = () => {
  const letters = '0123456789ABCDEF'.split('');
  return range(0, 6).reduce(color => color + letters[Math.floor(Math.random() * 16)], '#');
}

const groupColors = (photos: Photo[]) => {
  const groups = Array.from(new Set(photos.map(p => p.group).filter(x => !!x)));

  return groups.reduce((style, group) => ({
    ...style,
    [group]: getRandomColor(),
  }), {});
}

const PhotoProvider: React.FunctionComponent<Props> = ({ category, children }) => {
  const { updateCategory } = React.useContext(CategoryContext);
  const { photoService, categoryService } = React.useContext(ServiceContext);
  const [ groups, setGroups ] = React.useState<Record<string, string>>({});
  const [ photos, setPhotos ] = React.useState<Record<string, Photo>>({});
  const [ selection, setSelection ] = React.useState([]);

  const loadPhotos = () => {
    categoryService.fetchPhotos(category)
      .then((photos: Photo[]) => indexBy(photo => String(photo.id), photos))
      .then((photos: Record<string, Photo>) => {
        setPhotos(photos);
        setGroups(groupColors(values(photos)));
      });
  }

  React.useEffect(loadPhotos, [])

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

  const getPhotos: GetPhotos = (page = 1, limit = 50) => slice(
    (page - 1) * limit,
    page * limit,
    values(photos),
  )

  const deletePhotos: DeletePhotos = (photos) => {
    categoryService
      .unlinkPhotos(category, photos)
      .then(() => {
        cleanSelection();
        loadPhotos();
      });
  }

  const updatePhoto = async (photo: Photo) => {
    const photo$ = await photoService.update(photo);

    setPhotos(photos => ({
      ...photos,
      [photo.id]: photo$,
    }));
  };

  const makeFeatured = (photo: Photo) => {
    updateCategory({
      ...category,
      featured: photo,
    });

    cleanSelection();
  }

  const toggleVisibility = (photos: Photo[]) => {
    photos.forEach((photo) => {
      updatePhoto({
        ...photo,
        hidden: !photo.hidden
      });
    });

    cleanSelection();
  }

  const ungroup = (photo: Photo) => {
    photoService
      .removeGroup(photo.group, [photo])
      .then(() => {
        cleanSelection();
        loadPhotos();
      });
  }

  const group = async (photos: Photo[]) => {
    const photo = photos.find(p => !!p.group);
    await photo
      ? photoService.appendGroup(photo.group, photos)
      : photoService.group(photos)
    ;

    cleanSelection();
    loadPhotos();
  }

  return (
    <Context.Provider
      value={{
        getTotal: () => values(photos).length,
        getPhotos,
        getPhoto: (id: number) => photos[id],
        getGroup: (photo: Photo) => groups[photo.group],
        cleanSelection,
        isSelected,
        select,
        getSelectionCount,
        deletePhotos,
        updatePhoto,
        toggleVisibility,
        makeFeatured,
        ungroup,
        group,
      }}
    >
      {children}
    </Context.Provider>
  );
}

export default PhotoProvider;
