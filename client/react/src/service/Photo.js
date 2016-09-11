import fetch from 'isomorphic-fetch';
import config from '../config/config.json';
import Service from './BaseService';

const sizes = [
  (config.brickWidth)
, (config.brickWidth * 2 + config.gutter)
, (config.brickWidth * 3 + config.gutter * 2)
, (config.brickWidth * 4 + config.gutter * 3)
];

export default class PhotoService extends Service {

  getRandomColor () {
    const letters = '0123456789ABCDEF'.split('');
    let color = '#';
    for (let i = 0; i < 6; i++ ) {
      color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
  }

  fetchPhotos (category, showHidden) {
    const me = this,
      url = me.baseUrl() + '/category/' + category + '/photo';
        // url = new URL('/api/v1/category/' + category + '/photo', location.origin);
        // url.searchParams.append('hidden', showHidden);

    return fetch(url, {
      headers: me.headers
    })
    .then(this.respondJSON);
  }

  fetchPhoto (photoId) {
    const me = this;

    return fetch(me.baseUrl() + '/photo/' + photoId, {
      headers: me.headers
    })
    .then(this.respondJSON);
  }

  groupColors(photos) {
    const
      style = {},
      groups = [...new Set(photos.map(p => p.group).filter(x => !!x))];

    groups.map(g => style[g] = this.getRandomColor());

    return style;
  }

  patchPhoto(photo, props) {
    const me = this;

    return fetch(me.baseUrl() + '/photo/' + photo.id, {
      method: 'PATCH',
      headers: me.headers,
      body: JSON.stringify(props)
    });
  }

  updateParents(photos, parent, showHidden) {
    const me = this;

    return this.fetchPhotos(parent, showHidden)
      .then(parents => {
        return photos.map(photo => {
          photo.hasParent = parents.find(p => p.id === photo.id);
          return photo;
        });
      });
  }

  group(photos) {
    const me = this;

    return fetch(me.baseUrl() + '/photo/group', {
      method: 'POST',
      headers: me.headers,
      body: JSON.stringify(photos.map(p => p.id))
    });
  }

  appendGroup(groupId, photos) {
    const me = this;

    return fetch(me.baseUrl() + '/photo/group/' + groupId, {
      method: 'LINK',
      headers: me.headers,
      body: JSON.stringify(photos.map(p => p.id))
    });
  }

  removeGroup(groupId, photos) {
    const me = this;

    return fetch(me.baseUrl() + '/photo/group/' + groupId, {
      method: 'UNLINK',
      headers: me.headers,
      body: JSON.stringify(photos.map(p => p.id))
    });
  }

  dsmap(mode, ratio, isHorisontal) {
    const
      [s1,s2,s3,s4] = sizes,
      modes = [
        [ratio >= 2   ? s2 : s1, s1],
        isHorisontal ? [ratio >= 3 ? s3 : s2, s1] : [s1, s2],
        ratio >= 4   ? [s4, s1] : [ratio >= 2 ? s3 : s2, s2],
        isHorisontal ? [ratio >= 2 ? s4 : s3, s2] : [s2, s3]
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
      norm = [16,8,4,1].map(i => i * (Math.floor(avg) + 1)),
      norm$ = [1,2,3,4].map(i => Math.floor(i * std * v / avg) + 1),
      probs = norm.map((n,i) => n + norm$[i]),
      mode = this.weightedRandom(probs),
      isHorisontal = (photo.width > photo.height),
      ratio = photo.width / photo.height,
      [w, h] = this.dsmap(mode, ratio, isHorisontal)
      ;

    return Object.assign(photo, {
      w,
      h,
      ratio
    });
  }

  weightedRandom = function (probabilities) {
    const probabilitiesMap = probabilities.reduce((acc, v) => {
        acc.push(v + (acc.length ? acc[acc.length - 1] : 0));
        return acc;
      }, []),
      pointer = Math.floor(Math.random() * probabilitiesMap[probabilitiesMap.length - 1]);

    return probabilitiesMap.reduce((acc, v) => pointer <= v ? acc : ++acc, 0);
  }

  refinePhotos(photos, excludeId) {
    photos.reduce((i,p) => {
      p.order = i++;
      return i;
    }, 0);

    const
      exclude = photos.find(p => p.id === excludeId),

      // spread list on grouped and not grouped photos
      [init, grouped] = photos.reduce((acc, p) => {
        const [i,r] = acc;
        p.group ? r.push(p) : i.push(p);
        return acc;
      }, [[],[]]),

      groups = grouped.reduce((m,p) => {
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

    return init.sort((a,b) => a.order - b.order);
  }
}
