import photo from '../../../common/service/photo/memoize';
import config from '../etc/config';

export const localeURL = /^(\/)?(ru|en)?($|\/.*$)$/g;

export default {
  getSrc(...args) {
    return [config.staticEndpoint, photo.getSrc(...args)].join('/');
  },
};
