import { SET_RUNTIME_VARIABLE, INIT } from './actions';

import config from '../../../../client/src/etc/config.json';
import PhotoService from '../../../../client/lib/service/Photo';
import CategoryService from '../../../../client/lib/service/Category';
import TranslationService from '../../../../client/lib/service/Translation';

const initial = {};

config.apiEndpoint = '/api/v1'; //TODO: fix with mergeable configs

const init = (state, { token }) => Object.assign(state, {
  photoService: new PhotoService({
    apiEndpoint: config.apiEndpoint,
    token,
  }),
  categoryService: new CategoryService({
    apiEndpoint: config.apiEndpoint,
    token,
  }),
  translationService: new TranslationService({
    apiEndpoint: config.apiEndpoint,
    token,
  }),
});

export default function runtime(state = initial, action) {
  switch (action.type) {
    case SET_RUNTIME_VARIABLE:
      return Object.assign({}, state, {
        [action.name]: action.value,
      });
    case INIT:
      return init(state, action);
    default:
      return state;
  }
}
