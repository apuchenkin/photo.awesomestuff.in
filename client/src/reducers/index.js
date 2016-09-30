import { combineReducers } from 'redux';
import loader from './loader';
import runtime from './runtime';

export default combineReducers({
  runtime,
  isLoading: loader,
});
