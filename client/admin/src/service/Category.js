
const CategoryService = class {

  constructor(token) {
    this.token = token;
  }

  fetchCategories () {
    let me = this,
        url = new URL('/api/v1/category', location.origin);

    url.searchParams.append('hidden', true);

    return fetch(url, {
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
    return fetch('/api/v1/category/' + category.id + '/photo', {
        method: 'LINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  unlinkPhotos(category, photos) {
    return fetch('/api/v1/category/' + category.id + '/photo', {
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
