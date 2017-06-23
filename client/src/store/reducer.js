import { combineReducers } from 'redux';
import foundReducer from 'found/lib/foundReducer';
import isLoading from './loader/reducer';
import runtime from './runtime/reducer';
import category from './category/reducer';

export default combineReducers({
  found: foundReducer,
  runtime,
  isLoading,
  category,
});
