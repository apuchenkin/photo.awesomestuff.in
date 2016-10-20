import fetch from 'isomorphic-fetch';
import Service from './BaseService';

import { weightedRandom, refinePhotos, remapPhotos } from '../util/photo';

export default class PhotoService extends Service {
  getRandomColor() {
    const letters = '0123456789ABCDEF'.split('');
    return Array(6).reduce(color => color + letters[Math.floor(Math.random() * 16)], '#');
  }

  fetchPhotos(category) {
    const url = `${this.baseUrl()}/category/${category}/photo`;

    return fetch(url, {
      headers: this.headers,
    })
    .then(this.respondJSON);
  }

  fetchPhoto(photoId) {
    return fetch(`${this.baseUrl()}/photo/${photoId}`, {
      headers: this.headers,
    })
    .then(this.respondJSON);
  }

  groupColors(photos) {
    const groups = [...new Set(photos.map(p => p.group).filter(x => !!x))];

    return groups.reduce((style, g) => Object.assign(style, { [g]: this.getRandomColor() }), {});
  }

  patchPhoto(photo, props) {
    return fetch(`${this.baseUrl()}/photo/${photo.id}`, {
      method: 'PATCH',
      headers: this.headers,
      body: JSON.stringify(props),
    });
  }

  updateParents(photos, parent, showHidden) {
    return this.fetchPhotos(parent, showHidden)
      .then(parents => photos.map(photo => Object.assign(photo, {
        hasParent: parents.find(p => p.id === photo.id),
      })));
  }

  group(photos) {
    const me = this;

    return fetch(`${this.baseUrl()}/photo/group`, {
      method: 'POST',
      headers: me.headers,
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  appendGroup(groupId, photos) {
    return fetch(`${this.baseUrl()}/photo/group/${groupId}`, {
      method: 'LINK',
      headers: this.headers,
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  removeGroup(groupId, photos) {
    return fetch(`${this.baseUrl()}/photo/group/${groupId}`, {
      method: 'UNLINK',
      headers: this.headers,
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  static weightedRandom = weightedRandom;
  static refinePhotos = refinePhotos;
  static remapPhotos = remapPhotos;
}
