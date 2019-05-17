import * as React from 'react';
import { Context as ServiceContext } from './service';
import { values, indexBy, prop, slice } from 'ramda';

export type GetTotal = () => number;
export type GetPhotos = (page?: number, limit?: number) => Photo[];
export type GetGroups = () => any[];
export type GetPhoto = (id: number) => Photo | undefined;
export type DeletePhotos = (photos: Photo[]) => void;

interface ContextProps {
  getTotal: GetTotal;
  getPhotos: GetPhotos;
  getPhoto: GetPhoto;
  getGroups: GetGroups;
  deletePhotos: DeletePhotos;
}

// @ts-ignore
export const Context = React.createContext<ContextProps>();

interface Props {
  category: Category;
}

const PhotoProvider: React.FunctionComponent<Props> = ({ category, children }) => {
  const { categoryService } = React.useContext(ServiceContext);
  const [ photos, setPhotos ] = React.useState<Record<string, Photo>>({});
  const [ selection, setSelection ] = React.useState([]);
  const loadPhotos = () => {
    categoryService.fetchPhotos(category)
      .then(indexBy(prop<string, string>('id')))
      .then(setPhotos);
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

  // makeFeatured(photo) {
  //   this.props.updateCategory(this.props.category, {
  //     featuredId: photo.id,
  //   });
  //   this.cleanSelection();
  // }

  // toggleVisibility() {
  //   const { selection } = this.state;
  //   selection.forEach((photo) => {
  //     this.props.updatePhoto(photo, { hidden: !photo.hidden });
  //   });

  //   this.cleanSelection();
  // }

  // ungroup(photo) {
  //   this.props.photoService
  //     .removeGroup(photo.group, [photo])
  //     .then(() => {
  //       this.cleanSelection();
  //       this.props.loadPhotos(this.props.category);
  //     });
  // }

  // group(photos) {
  //   const photoService = this.props.photoService;
  //   const photo = photos.find(p => !!p.group);
  //   const promise = photo
  //     ? photoService.appendGroup(photo.group, photos)
  //     : photoService.group(photos)
  //   ;

  //   promise.then(() => {
  //     this.cleanSelection();
  //     this.props.loadPhotos(this.props.category);
  //   });
  // }

  return (
    <Context.Provider
      value={{
        getTotal: () => values(photos).length,
        getPhotos,
        getPhoto: (id: number) => photos[id],
        getGroups: (): any[] => [],
        cleanSelection,
        isSelected,
        select,
        getSelectionCount,
        deletePhotos,
      }}
    >
      {children}
    </Context.Provider>
  );
}

export default PhotoProvider;
