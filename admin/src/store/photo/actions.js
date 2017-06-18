export const LOAD_PHOTOS = 'LOAD_PHOTOS';
export const PHOTOS_LOADED = 'PHOTOS_LOADED';
export const ADD_PHOTO = 'ADD_PHOTO';

export const loadPhotos = category => ({
  type: LOAD_PHOTOS,
  category,
});

export const photosLoaded = photos => ({
  type: PHOTOS_LOADED,
  photos,
});

export const photoAdd = photo => ({
  type: ADD_PHOTO,
  photo,
});
