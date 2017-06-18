import { SET_RUNTIME_VARIABLE } from './actions';

import config from '../../../../client/src/etc/config.json';
import PhotoService from '../../../../client/lib/service/Photo';
import CategoryService from '../../../../client/lib/service/Category';

const initial = {
  photoService: new PhotoService({
    apiEndpoint: config.apiEndpoint,
  }),
  categoryService: new CategoryService({
    apiEndpoint: config.apiEndpoint,
  }),
};

export default function runtime(state = initial, action) {
  switch (action.type) {
    case SET_RUNTIME_VARIABLE:
      return Object.assign({}, state, {
        [action.name]: action.value,
      });
    default:
      return state;
  }
}
