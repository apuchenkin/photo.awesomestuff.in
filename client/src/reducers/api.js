import { GET_CATEGORIES, GET_PAGES } from '../actions/api';

export default function loading(state = {}, action) {
  switch (action.type) {
    case GET_CATEGORIES:
      return Object.assign(state, { categories: action.payload });
    case GET_PAGES:
      return Object.assign(state, { pages: action.payload });
    default:
      return state;
  }
}
