import fetch from 'isomorphic-fetch';
import config from '../config.json';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

const defaults = {
  locale: config.fallbackLocale,
  location: config.apiEndpoint,
  contentType: 'application/json; charset=utf-8'
};


export default class CategoryService {

  constructor(options = {}) {
    Object.assign(this, defaults, options);
  }

  baseUrl() {
    return this.location + config.apiPrefix + '/page';
  }

  fetchPages () {
    let me = this;

    return fetch(me.baseUrl(), {
      headers: {
        'Authorization': me.token,
        'Accept-Language': me.locale,
        'Content-Type': me.contentType
      }
    })
    .then(response => {
      return response.text();
    })
    .then(stream => {
      return JSON.parse(stream);
    });
  }

  fetchPage (pageId) {
    let me = this;

    return fetch(me.baseUrl() + '/' + pageId, {
      headers: {
        'Authorization': me.token,
        'Accept-Language': me.locale,
        'Content-Type': me.contentType
      }
    })
    .then(response => {
      return response.text();
    })
    .then(stream => {
      return JSON.parse(stream);
    });
  }
}
