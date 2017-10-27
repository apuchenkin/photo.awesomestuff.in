import { combineReducers } from 'redux';
import foundReducer from 'found/lib/foundReducer';
// import isLoading from './loader/reducer';
import runtime from './runtime/reducer';
// import category from './category/reducer';
import page from './page/reducer';
import photo from './photo/reducer';

export default combineReducers({
  found: foundReducer,
  // categories: [],
  runtime,
  // isLoading,
  category: (state = { categories: [] }) => state,
  page,
  photo,
});
