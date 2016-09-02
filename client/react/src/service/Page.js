import fetch from 'isomorphic-fetch';
import config from '../config.json';
import Service from './BaseService';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

export default class PageService extends Service {

  fetchPages () {
    const me = this;

    return fetch(me.baseUrl() + '/page', {
      headers: this.headers
    })
    .then(this.respondJSON);
  }

  fetchPage (pageId) {
    const me = this;

    return fetch(me.baseUrl() + '/page/' + pageId, {
      headers: this.headers
    })
    .then(this.respondJSON);
  }
}
