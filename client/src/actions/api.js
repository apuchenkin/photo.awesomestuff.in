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
      return {
        type: GET_CATEGORIES,
        payload: categoryService.fetchCategories(),
      };
    },

    getCategory(categoryName) {
      return {
        type: GET_CATEGORY,
        payload: categoryService.fetchCategory(categoryName),
      };
    },

    getPages() {
      return {
        type: GET_PAGES,
        payload: pageService.fetchPages(),
      };
    },

    getPage(pageId) {
      return {
        type: GET_PAGE,
        payload: pageService.fetchPage(pageId),
      };
    },

    getPhotos(categoryName) {
      return {
        type: GET_PHOTOS,
        payload: photoService.fetchPhotos(categoryName),
      };
    },

    getPhoto(photoId) {
      return {
        type: GET_PHOTO,
        payload: photoService.fetchPhoto(photoId),
      };
    },
  };
};
