import Service from './BaseService';

import { weightedRandom, refinePhotos, remapPhotos } from '../util/photo';

export { weightedRandom, refinePhotos, remapPhotos };

export default class PhotoService extends Service {

  baseUrl() {
    return `${super.baseUrl()}/photo`;
  }

  fetchPhoto(photoId) {
    return this.fetch(`/${photoId}`)
    .then(Service.respondJSON);
  }

  patchPhoto(photoId, diff) {
    return this.fetch(`/${photoId}`, {
      method: 'PATCH',
      body: JSON.stringify(diff),
    });
  }

  // updateParents(photos, parent, showHidden) {
  //   return this.fetchPhotos(parent, showHidden)
  //     .then(parents => photos.map(photo => Object.assign(photo, {
  //       hasParent: parents.find(p => p.id === photo.id),
  //     })));
  // }

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
    return Array(6).reduce(color => color + letters[Math.floor(Math.random() * 16)], '#');
  }

  static groupColors(photos) {
    const groups = [...new Set(photos.map(p => p.group).filter(x => !!x))];

    return groups.reduce((style, g) => Object.assign(style, {
      [g]: PhotoService.getRandomColor(),
    }), {});
  }
}
