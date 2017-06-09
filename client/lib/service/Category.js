import Service from './BaseService';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;
// url.searchParams.append('hidden', true);

export default class CategoryService extends Service {

  baseUrl() {
    return `${super.baseUrl()}/category`;
  }

  fetchCategories() {
    return this.fetch('')
      .then(Service.respondJSON)
      .then(CategoryService.refineCategories);
  }

  fetchCategory(name) {
    return this.fetch(`/${name}`)
      .then(Service.respondJSON);
  }

  fetchPhotos(categoryName) {
    return this.fetch(`/${categoryName}/photo`)
      .then(Service.respondJSON);
  }

  fetchTranslations(categoryName) {
    return this.fetch(`/${categoryName}/translation`)
      .then(Service.respondJSON);
  }

  create(category) {
    return this.fetch('', {
      method: 'POST',
      body: JSON.stringify(category),
    });
  }

  delete(categoryName) {
    return this.fetch(`/${categoryName}`, {
      method: 'DELETE',
    });
  }

  update(categoryName, diff) {
    return this.fetch(`/${categoryName}`, {
      method: 'PATCH',
      body: JSON.stringify(diff),
    });
  }

  createTranslation(categoryName, data) {
    return this.fetch(`/${categoryName}/translation`, {
      method: 'POST',
      body: JSON.stringify(data),
    }).then(Service.respondJSON);
  }

  deleteTranslation(categoryName, translationId) {
    return this.fetch(`/${categoryName}/translation/${translationId}`, {
      method: 'DELETE',
    });
  }

  linkPhotos(categoryName, photos) {
    return this.fetch(`/${categoryName}/photo`, {
      method: 'LINK',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  unlinkPhotos(categoryName, photos) {
    return this.fetch(`/${categoryName}/photo`, {
      method: 'UNLINK',
      body: JSON.stringify(photos.map(p => p.id)),
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
}
