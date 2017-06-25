import { combineEpics } from 'redux-observable';
import category from './category/epic';
import page from './page/epic';

export default combineEpics(
  category,
  page,
);
