import Immutable from 'seamless-immutable';

import { LOADED, CREATED, UPDATED, REMOVED, ERROR } from './actions';

const initial = Immutable({
  translations: [],
});

const loaded = (state, { translations }) => state.set('translations', Immutable(translations));
const created = (state, { translation }) => state.set('translations', Immutable([...state.translations, translation]));
const updated = (state, { translation }) => state.set('translations',
  state.translations.set(state.translations.findIndex(c => c.id === translation.id), translation),
);
const removed = (state, { translation }) =>
  state.set('translations', Immutable(state.translations.filter(c => c.id !== translation.id)));

const error = (state, action) => {
  console.log(action.error); // eslint-disable-line no-console
  return state;
};

export default (state = initial, action) => {
  const reducer = {
    [LOADED]: loaded,
    [CREATED]: created,
    [UPDATED]: updated,
    [REMOVED]: removed,
    [ERROR]: error,
  }[action.type];

  return reducer ? reducer(state, action) : state;
};
