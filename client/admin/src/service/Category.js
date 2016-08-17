require('isomorphic-fetch');
// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

const CategoryService = class {

  constructor(token, location) {
    this.token = token;
    this.location = location;
  }

  fetchCategories () {
    let me = this;
    // url.searchParams.append('hidden', true);
    
    return fetch(me.location + '/api/v1/category', {
        headers: {
          'Authorization': me.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
      })
      .then(response => {
        return response.text();
      })
      .then(stream => {
        return JSON.parse(stream);
      })
  }

  linkPhotos(category, photos) {
    return fetch('//api/v1/category/' + category.id + '/photo', {
        method: 'LINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  unlinkPhotos(category, photos) {
    return fetch('//api/v1/category/' + category.id + '/photo', {
        method: 'UNLINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }
};

export default CategoryService;
