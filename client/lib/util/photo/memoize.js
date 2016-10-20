import memoize from 'memoizee';

import { getSrc, adjust } from '../photo';

module.exports = {
  getSrc: memoize(getSrc),
  adjust: memoize(adjust),
};
