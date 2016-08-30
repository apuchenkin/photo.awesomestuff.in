import fetch from 'isomorphic-fetch';
import config from '../config.json';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

const defaults = {
  locale: config.fallbackLocale,
  location: config.apiEndpoint
};

export default class CategoryService {

  constructor(options = defaults) {
    this.token = options.token;
    this.location = options.location;
    this.locale = options.locale;
    this.contentType = 'application/json; charset=utf-8';
  }

  fetchCategories () {
    let me = this;
    // url.searchParams.append('hidden', true);

    return fetch(me.location + config.apiPrefix + '/category', {
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
    })
    .then(categories => {
      return me.refineCategories(categories);
    });
  }

  refineCategories (categories) {
    let map = new Map(categories.map(c => [c.id, c]));

    //setting parent for all categories
    map.forEach(category => {
      category.parent = category.parent && map.get(category.parent);
    });

    //setting childs
    map.forEach(category => {
      category.childs = categories.filter(c => c.parent && c.parent.id === category.id).map(c => c.id);
    });

    return categories;
  }

  linkPhotos(category, photos) {
    const me = this;

    return fetch(config.apiPrefix + '/category/' + category.id + '/photo', {
      method: 'LINK',
      headers: {
        'Authorization': me.token,
        'Content-Type': me.contentType
      },
      body: JSON.stringify(photos.map(p => p.id))
    });
  }

  unlinkPhotos(category, photos) {
    const me = this;

    return fetch(config.apiPrefix + '/category/' + category.id + '/photo', {
      method: 'UNLINK',
      headers: {
        'Authorization': me.token,
        'Content-Type': me.contentType
      },
      body: JSON.stringify(photos.map(p => p.id))
    });
  }
}
