import { combineReducers } from 'redux';
import foundReducer from 'found/lib/foundReducer';
import runtime from './runtime/reducer';
import page from './page/reducer';
import photo from './photo/reducer';
import { reducer as cache } from './cache';

export default combineReducers({
  found: foundReducer,
  runtime,
  category: (state = { categories: [] }) => state,
  page,
  photo,
  cache,
});
