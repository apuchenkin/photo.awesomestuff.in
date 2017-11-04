import { combineReducers } from 'redux';
import foundReducer from 'found/lib/foundReducer';
import runtime from './runtime/reducer';
import { reducer as cache } from './cache';

export default combineReducers({
  found: foundReducer,
  runtime,
  cache,
  categories: (state = []) => state,
  pages: (state = []) => state,
});
