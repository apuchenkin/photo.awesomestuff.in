import { combineEpics } from 'redux-observable';
import category from './category/epic';

export default combineEpics(
  category,
);
