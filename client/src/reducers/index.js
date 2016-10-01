import { combineReducers } from 'redux';
import isLoading from './loader';
import runtime from './runtime';
import api from './api';

export default combineReducers({
  runtime,
  isLoading,
  api,
});
