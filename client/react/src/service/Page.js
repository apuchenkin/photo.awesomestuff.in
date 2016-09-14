import fetch from 'isomorphic-fetch';
import Service from './BaseService';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

export default class PageService extends Service {

  fetchPages() {
    return fetch(`${this.baseUrl()}/page`, {
      headers: this.headers,
    })
    .then(this.respondJSON);
  }

  fetchPage(pageId) {
    return fetch(`${this.baseUrl()}/page/${pageId}`, {
      headers: this.headers,
    })
    .then(this.respondJSON);
  }
}
