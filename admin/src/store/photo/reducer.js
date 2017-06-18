import Immutable from 'seamless-immutable';

import { LOAD_PHOTOS, PHOTOS_LOADED, ADD_PHOTO } from './actions';
import PhotoService from '../../../../client/lib/service/Photo';

const initial = Immutable({
  photos: [],
  groups: [],
  category: null,
});

const loadPhotos = (state, { category }) => state.set('category', category);
const photosLoaded = (state, { photos }) => state
  .set('photos', photos)
  .set('groups', PhotoService.groupColors(photos));

const photoAdd = (state, { photo }) => state.set('photos', state.photos.push(photo));

export default (state = initial, action) => {
  const reducer = {
    [LOAD_PHOTOS]: loadPhotos,
    [PHOTOS_LOADED]: photosLoaded,
    [ADD_PHOTO]: photoAdd,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};

// const parent = categories.find(c => c.id === category);
//
// if (parent && parent.parent) {
//   photoService.updateParents(photos, parent.parent).then(this.setPhotos);
// } else {
//   this.setPhotos(photos);
// }
