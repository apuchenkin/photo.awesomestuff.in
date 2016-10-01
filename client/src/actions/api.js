import CategoryService from '../service/Category';
import PageService from '../service/Page';

export const GET_CATEGORIES = 'GET_CATEGORIES';
export const GET_PAGES = 'GET_PAGES';

const categoryService = new CategoryService();
const pageService = new PageService();

export function getCategories() {
  return {
    type: GET_CATEGORIES,
    payload: categoryService.fetchCategories(),
  };
}

export function getPages() {
  return {
    type: GET_PAGES,
    payload: pageService.fetchPages(),
  };
}
