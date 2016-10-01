import CategoryService from '../service/Category';
import PageService from '../service/Page';
import PhotoService from '../service/Photo';

export const GET_CATEGORIES = 'GET_CATEGORIES';
export const GET_CATEGORY = 'GET_CATEGORY';

export const GET_PAGES = 'GET_PAGES';
export const GET_PAGE = 'GET_PAGE';

export const GET_PHOTOS = 'GET_PHOTOS';
export const GET_PHOTO = 'GET_PHOTO';

const categoryService = new CategoryService();
const pageService = new PageService();
const photoService = new PhotoService();

export function getCategories() {
  return {
    type: GET_CATEGORIES,
    payload: categoryService.fetchCategories(),
  };
}

export function getCategory(categoryName) {
  return {
    type: GET_CATEGORY,
    payload: categoryService.fetchCategory(categoryName),
  };
}

export function getPages() {
  return {
    type: GET_PAGES,
    payload: pageService.fetchPages(),
  };
}

export function getPage(pageId) {
  return {
    type: GET_PAGE,
    payload: pageService.fetchPage(pageId),
  };
}

export function getPhotos(categoryName) {
  return {
    type: GET_PHOTOS,
    payload: photoService.fetchPhotos(categoryName),
  };
}

export function getPhoto(photoId) {
  return {
    type: GET_PHOTO,
    payload: photoService.fetchPhoto(photoId),
  };
}
