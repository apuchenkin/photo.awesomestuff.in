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
      return Object.assign(state, { categories: action.payload.filter(c => !!c.title) });
    case GET_CATEGORY:
      return Object.assign(state, { category:
         CategoryService.attachParent(action.payload, state.categories),
       });
    case GET_PAGES:
      return Object.assign(state, { pages: action.payload.filter(c => !!c.title) });
    case GET_PAGE:
      return Object.assign(state, { page: action.payload });
    case GET_PHOTOS:
      return Object.assign(state, { photos: action.payload });
    case GET_PHOTO:
      return Object.assign(state, { photo: action.payload });
    default:
      return state;
  }
}
