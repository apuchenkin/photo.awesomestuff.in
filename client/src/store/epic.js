import { combineEpics } from 'redux-observable';
import category from './category/epic';
import page from './page/epic';
import photo from './photo/epic';

export default combineEpics(
  category,
  page,
  photo,
);
