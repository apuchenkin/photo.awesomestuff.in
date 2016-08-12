
const PhotoService = class {

  constructor(token) {
    this.token = token;
  }

  getRandomColor () {
    var letters = '0123456789ABCDEF'.split('');
    var color = '#';
    for (var i = 0; i < 6; i++ ) {
      color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
  };

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

  groupColors(photos) {
    let
      style = {},
      groups = [...new Set(photos.map(p => p.group).filter(x => !!x))];

    groups.map(g => style[g] = this.getRandomColor());

    return style;
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

  group(photos) {
    return fetch('/api/v1/photo/group', {
        method: 'POST',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  appendGroup(groupId, photos) {
    return fetch('/api/v1/photo/group/' + groupId, {
        method: 'LINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  removeGroup(groupId, photos) {
    return fetch('/api/v1/photo/group/' + groupId, {
        method: 'UNLINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }
};

export default PhotoService;
