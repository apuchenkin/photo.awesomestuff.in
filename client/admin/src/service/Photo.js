
const PhotoService = class {

  constructor(token) {
    this.token = token;
  }

  fetchPhotos (category, showHidden) {
    let me = this,
        url = new URL('/api/v1/category/' + category + '/photo', location.origin);

    url.searchParams.append('hidden', showHidden);

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

  updateParents(photos, parent, showHidden) {
    return this.fetchPhotos(parent, showHidden)
      .then(parents => {
        return photos.map(photo => {
          photo.hasParent = parents.find(p => p.id == photo.id);
          return photo;
        });
      });
  }
};

export default PhotoService;
