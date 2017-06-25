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
      .then(Service.respondJSON);
      // .then(CategoryService.refineCategories);
  }

  fetchCategory(name) {
    return this.fetch(`/${name}`)
      .then(Service.respondJSON);
  }

  fetchPhotos(category) {
    return this.fetch(`/${category.name}/photo`)
      .then(Service.respondJSON);
  }

  create(category) {
    return this.fetch('', {
      method: 'POST',
      body: JSON.stringify(category),
    }).then(Service.respondJSON);
  }

  delete(category) {
    return this.fetch(`/${category.name}`, {
      method: 'DELETE',
    });
  }

  update(category, diff) {
    return this.fetch(`/${category.name}`, {
      method: 'PATCH',
      body: JSON.stringify(diff),
    }).then(Service.respondJSON);
  }

  fetchTranslations(category) {
    return this.fetch(`/${category.name}/translation`)
      .then(Service.respondJSON);
  }

  createTranslation(category, data) {
    return this.fetch(`/${category.name}/translation`, {
      method: 'POST',
      body: JSON.stringify(data),
    }).then(Service.respondJSON);
  }

  updateTranslation(category, translation, diff) {
    return this.fetch(`/${category.name}/translation/${translation.id}`, {
      method: 'PATCH',
      body: JSON.stringify(diff),
    }).then(Service.respondJSON);
  }


  deleteTranslation(category, translation) {
    return this.fetch(`/${category.name}/translation/${translation.id}`, {
      method: 'DELETE',
    });
  }

  linkPhotos(category, photos) {
    return this.fetch(`/${category.name}/photo`, {
      method: 'LINK',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  unlinkPhotos(category, photos) {
    return this.fetch(`/${category.name}/photo`, {
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
