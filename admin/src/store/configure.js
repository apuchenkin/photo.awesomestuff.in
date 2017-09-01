import {
  createStore,
  applyMiddleware,
  compose,
  combineReducers,
} from 'redux';
import { createEpicMiddleware, combineEpics } from 'redux-observable';

import categoryReducer from './category/reducer';
import runtimeReducer from './runtime/reducer';
import photoReducer from './photo/reducer';
import translationReducer from './translation/reducer';

import CategoryEpic from './category/epic';
import PhotoEpic from './photo/epic';
import TranslationEpic from './translation/epic';

// eslint-disable-next-line no-underscore-dangle
const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;

const epic = combineEpics(CategoryEpic, PhotoEpic, TranslationEpic);
const epicMiddleware = createEpicMiddleware(epic);

const reducer = combineReducers({
  category: categoryReducer,
  photo: photoReducer,
  translation: translationReducer,
  runtime: runtimeReducer,
});

const store = createStore(reducer,
  composeEnhancers(
    applyMiddleware(epicMiddleware),
  ),
);

export default store;
