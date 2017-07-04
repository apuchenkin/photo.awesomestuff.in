import { SET_RUNTIME_VARIABLE, INIT } from './actions';

import config from '../../../etc/config.json';
import PhotoService from '../../../../common/service/api/Photo';
import CategoryService from '../../../../common/service/api/Category';
import TranslationService from '../../../../common/service/api/Translation';

const initial = {};

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
