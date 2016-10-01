import { GET_CATEGORIES, GET_CATEGORY, GET_PAGES, GET_PAGE, GET_PHOTOS, GET_PHOTO } from '../actions/api';
import CategoryService from '../service/Category';

const intial = {
  categories: [],
  photos: [],
  pages: [],
};

export default function loading(state = intial, action) {
  if (action.error) {
    return state;
  }

  switch (action.type) {
    case GET_CATEGORIES:
      return { ...state, categories: action.payload.filter(c => !!c.title) };
    case GET_CATEGORY:
      return { ...state,
        category: CategoryService.attachParent(action.payload, state.categories),
       };
    case GET_PAGES:
      return { ...state, pages: action.payload.filter(c => !!c.title) };
    case GET_PAGE:
      return { ...state, page: action.payload };
    case GET_PHOTOS:
      return { ...state, photos: action.payload };
    case GET_PHOTO:
      return { ...state, photo: action.payload };
    default:
      return state;
  }
}
