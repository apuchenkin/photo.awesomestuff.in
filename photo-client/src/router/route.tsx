import * as React from 'react';
import Route from 'found/lib/Route';
// @ts-ignore
import shallowequal from 'shallowequal';
import { RouteProps, RouteMatch } from 'found';

class AsyncRoute extends Route {
  path?: string;
  getData?: (match: RouteMatch) => any

  constructor(props: RouteProps) {
    super(props);
    if (this.getData) {
      this.getData = this.getAsyncData(this.getData);
    }
  }

  getAsyncData = (getData: (match: RouteMatch) => any) => (match: RouteMatch) => {
    const data = match.context.data || {};
    const cache = this.path && data[this.path];

    if (cache && shallowequal(cache && cache.params, match.routeParams)) {
      return cache.data;
    }

    return getData(match);
  }
}

export default AsyncRoute;