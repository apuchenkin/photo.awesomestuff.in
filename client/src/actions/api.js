import CategoryService from '../service/Category';
import PageService from '../service/Page';
import PhotoService from '../service/Photo';

export const GET_CATEGORIES = 'GET_CATEGORIES';
export const GET_CATEGORY = 'GET_CATEGORY';

export const GET_PAGES = 'GET_PAGES';
export const GET_PAGE = 'GET_PAGE';

export const GET_PHOTOS = 'GET_PHOTOS';
export const GET_PHOTO = 'GET_PHOTO';

export default (store) => {
  const { locale } = store.getState().runtime;

  const categoryService = new CategoryService({ locale });
  const pageService = new PageService({ locale });
  const photoService = new PhotoService({ locale });

  return {
    getCategories() {
      if (store.getState().api.categories &&
          store.getState().api.categories.length) {
        return false;
      }

      return {
        type: GET_CATEGORIES,
        payload: categoryService.fetchCategories(),
      };
    },

    getCategory(name) {
      if (store.getState().api.category &&
          store.getState().api.category.name === name) {
        return false;
      }

      return {
        type: GET_CATEGORY,
        payload: categoryService.fetchCategory(name),
      };
    },

    getPages() {
      if (store.getState().api.pages &&
          store.getState().api.pages.length) {
        return false;
      }

      return {
        type: GET_PAGES,
        payload: pageService.fetchPages(),
      };
    },

    getPage(pageId) {
      if (store.getState().api.page &&
          store.getState().api.page.id === pageId) {
        return false;
      }

      return {
        type: GET_PAGE,
        payload: pageService.fetchPage(pageId),
      };
    },

    getPhotos(categoryName) {
      if (store.getState().api.photos
       && store.getState().api.photos.length
       && store.getState().api.category
       && store.getState().api.category.name === categoryName
     ) {
        return false;
      }

      return {
        type: GET_PHOTOS,
        payload: photoService.fetchPhotos(categoryName),
      };
    },

    getPhoto(photoId) {
      if (store.getState().api.photo
       && store.getState().api.photo.id === photoId
      ) {
        return false;
      }

      return {
        type: GET_PHOTO,
        payload: photoService.fetchPhoto(photoId),
      };
    },
  };
};
