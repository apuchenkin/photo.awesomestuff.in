import Service from './BaseService';

import { weightedRandom, refinePhotos, remapPhotos } from '../photo';

export { weightedRandom, refinePhotos, remapPhotos };

export default class PhotoService extends Service {

  baseUrl() {
    return `${super.baseUrl()}/photo`;
  }

  fetchPhoto(photoId) {
    return this.fetch(`/${photoId}`)
    .then(Service.respondJSON);
  }

  update(photo, data) {
    return this.fetch(`/${photo.id}`, {
      method: 'PATCH',
      body: JSON.stringify(data),
    }).then(Service.respondJSON);
  }

  fetchTranslations(photo) {
    return this.fetch(`/${photo.id}/translation`)
      .then(Service.respondJSON);
  }

  createTranslation(photo, data) {
    return this.fetch(`/${photo.id}/translation`, {
      method: 'POST',
      body: JSON.stringify(data),
    }).then(Service.respondJSON);
  }

  updateTranslation(photo, translation, diff) {
    return this.fetch(`/${photo.id}/translation/${translation.id}`, {
      method: 'PATCH',
      body: JSON.stringify(diff),
    }).then(Service.respondJSON);
  }

  deleteTranslation(photo, translation) {
    return this.fetch(`/${photo.id}/translation/${translation.id}`, {
      method: 'DELETE',
    });
  }

  group(photos) {
    return this.fetch('/group', {
      method: 'POST',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  appendGroup(groupId, photos) {
    return this.fetch(`/group/${groupId}`, {
      method: 'LINK',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  removeGroup(groupId, photos) {
    return this.fetch(`/group/${groupId}`, {
      method: 'UNLINK',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  static getRandomColor() {
    const letters = '0123456789ABCDEF'.split('');
    return Array(6).fill().reduce(color => color + letters[Math.floor(Math.random() * 16)], '#');
  }

  static groupColors(photos) {
    const groups = Array.from(new Set(photos.map(p => p.group).filter(x => !!x)));

    return groups.reduce((style, g) => Object.assign(style, {
      [g]: PhotoService.getRandomColor(),
    }), {});
  }
}
