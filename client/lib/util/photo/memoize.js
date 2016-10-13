import memoize from 'memoizee';

import { getSrc, adjust } from '../photo';

export default {
  getSrc: memoize(getSrc),
  adjust: memoize(adjust),
};
