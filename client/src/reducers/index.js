import { combineReducers } from 'redux';
import loader from './loader';

export default combineReducers({
  isLoading: loader,
});
