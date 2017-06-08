import fetch from 'isomorphic-fetch';
import Service from './BaseService';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

export default class CategoryService extends Service {

  fetchCategories() {
    const url = `${this.baseUrl()}/category`;
    // url.searchParams.append('hidden', true);

    return fetch(url, {
      headers: this.headers,
    })
    .then(this.respondJSON)
    .then(CategoryService.refineCategories);
  }

  fetchCategory(name) {
    const url = `${this.baseUrl()}/category/${name}`;

    return fetch(url, {
      headers: this.headers,
    })
    .then(this.respondJSON);
  }

  create(category) {
    const url = `${this.baseUrl()}/category`;

    return fetch(url, {
      method: 'POST',
      headers: this.headers,
      body: JSON.stringify(category),
    });
  }

  static refineCategories(categories) {
    const map = new Map(categories.map(c => [c.id, c]));

    // setting parent for all categories
    map.forEach(category => Object.assign(category, {
      parent: category.parent && map.get(category.parent),
    }));

    // setting childs
    map.forEach(category => Object.assign(category, {
      childs: categories.filter(c => c.parent && c.parent.id === category.id).map(c => c.id),
    }));

    return categories;
  }

  static attachParent(category, categories) {
    return category.parent
      ? Object.assign(category, {
        parent: categories.find(c => c.id === category.parent),
      })
      : Object.assign(category, {
        childs: categories.filter(c => c.parent && c.parent.id === category.id).map(c => c.id),
      })
      ;
  }

  linkPhotos(category, photos) {
    const url = `${this.baseUrl()}/category/${category.id}/photo`;

    return fetch(url, {
      method: 'LINK',
      headers: this.headers,
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  unlinkPhotos(category, photos) {
    const url = `${this.baseUrl()}/category/${category.id}/photo`;

    return fetch(url, {
      method: 'UNLINK',
      headers: this.headers,
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }
}
