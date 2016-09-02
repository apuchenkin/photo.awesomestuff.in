import fetch from 'isomorphic-fetch';
import config from '../config.json';
import Service from './BaseService';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;


export default class CategoryService extends Service {

  fetchCategories() {
    const me = this,
      url = this.baseUrl() + '/category';
    // url.searchParams.append('hidden', true);

    return fetch(url, {
      headers: me.headers
    })
    .then(this.respondJSON)
    .then(categories => {
      return me.refineCategories(categories);
    });
  }

  refineCategories(categories) {
    const map = new Map(categories.map(c => [c.id, c]));

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
    const me = this,
      url = this.baseUrl() + '/category/' + category.id + '/photo';

    return fetch(url, {
      method: 'LINK',
      headers: this.headers,
      body: JSON.stringify(photos.map(p => p.id))
    });
  }

  unlinkPhotos(category, photos) {
    const me = this,
      url = this.baseUrl() + '/category/' + category.id + '/photo';

    return fetch(url, {
      method: 'UNLINK',
      headers: this.headers,
      body: JSON.stringify(photos.map(p => p.id))
    });
  }
}
