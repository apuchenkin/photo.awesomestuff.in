import photo from '../../../common/service/photo/memoize';
import config from '../etc/config';

export const localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g;

export default {
  fetchAll(object) {
    const keys = Object.keys(object);

    return Promise
      .all(keys.map(k => object[k]))
      .then(data => keys.reduce((acc, k, idx) => Object.assign(acc, { [k]: data[idx] }), {}));
  },

  getSrc(...args) {
    return [config.staticEndpoint, photo.getSrc(...args)].join('/');
  },
};
