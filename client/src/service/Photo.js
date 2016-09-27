import fetch from 'isomorphic-fetch';
import memoize from 'memoizee';
import { apiEndpoint, apiPrefix, brickWidth, gutter } from '../config/config.json';
import Service from './BaseService';

import resolutions from '../config/resolution.json';

const sizes = [
  (brickWidth)
, ((brickWidth * 2) + (gutter))
, ((brickWidth * 3) + (gutter * 2))
, ((brickWidth * 4) + (gutter * 3)),
];

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

  dsmap(mode, ratio, isHorisontal) {
    const
      [s1, s2, s3, s4] = sizes,
      modes = [
        [ratio >= 2 ? s2 : s1, s1],
        isHorisontal ? [ratio >= 3 ? s3 : s2, s1] : [s1, s2],
        ratio >= 4 ? [s4, s1] : [ratio >= 2 ? s3 : s2, s2],
        isHorisontal ? [ratio >= 2 ? s4 : s3, s2] : [s2, s3],
      ];

    return modes[mode];
  }

  remapPhotos(photos) {
    const avg = photos.reduce((sum, p) => sum + p.views, 0) / photos.length;
    return photos.map(this.remapPhoto.bind(this, avg));
  }

  remapPhoto(avg, photo) {
    const
      v = photo.views,
      std = Math.sqrt(Math.pow((v - avg), 2)),
      norm = [16, 8, 4, 1].map(i => i * (Math.floor(avg) + 1)),
      norm$ = [1, 2, 3, 4].map(i => Math.floor((i * std * v) / avg) + 1),
      probs = norm.map((n, i) => n + norm$[i]),
      mode = PhotoService.weightedRandom(probs),
      isHorisontal = (photo.width > photo.height),
      ratio = photo.width / photo.height,
      [w, h] = this.dsmap(mode, ratio, isHorisontal)
      ;

    return Object.assign(photo, {
      w,
      h,
    });
  }

  @memoize
  static weightedRandom(probabilities) {
    const probabilitiesMap = probabilities.reduce((acc, v) => {
        acc.push(v + (acc.length ? acc[acc.length - 1] : 0));
        return acc;
      }, []),
      pointer = Math.floor(Math.random() * probabilitiesMap[probabilitiesMap.length - 1]);

    return probabilitiesMap.reduce((acc, v) => (pointer <= v ? acc : acc + 1), 0);
  }

  @memoize
  static getSize(photo) {
    const
      { w, h, width, height } = photo,
      ratio = width / height,
      inc = ratio >= 1 ? ratio : 1 / ratio,
      [m1, m2] = w < h ? [Math.ceil(w * inc), h] : [Math.ceil(h * inc), w];

    return Math.max(m1, m2);
  }

  @memoize
  static getSrc(photo, dimensions) {
    const
      { width, height } = dimensions,
      [w, h] = this.adjust(width, height),
      filename = photo.src.split('/').pop();

    return [apiEndpoint + apiPrefix, 'hs/photo', photo.id, w, h, filename].join('/');
  }

  @memoize
  static adjust(w, h) {
    const
      norms = resolutions.map(([w$, h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
      min = Math.min(...norms),
      idx = norms.findIndex(n => n === min)
      ;

    return resolutions[idx];
  }

  refinePhotos(photos, excludeId) {
    photos.map((p, k) => Object.assign(p, { order: k }));

    const
      exclude = photos.find(p => p.id === excludeId),

      // spread list on grouped and not grouped photos
      [init, grouped] = photos.reduce((acc, p) => {
        const [i, r] = acc;
        Object.assign(p, { group: p.group ? r.push(p) : i.push(p) });
        return acc;
      }, [[], []]),

      groups = grouped.reduce((m, p) => {
        if (p.group) {
          const
          v = m.get(p.group) || [];

          v.push(p);
          m.set(p.group, v);
        }

        return m;
      }, new Map());

    groups.forEach((value, key) => {
      let item;

      if (exclude && exclude.group === key) {
        item = exclude;
      } else {
        item = value[Math.floor(Math.random() * value.length)];
      }

      item.views = value.reduce((sum, v) => sum + v.views, 0);
      init.push(item);
    });

    return init.sort((a, b) => a.order - b.order);
  }
}
