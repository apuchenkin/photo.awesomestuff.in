import fetch from 'isomorphic-fetch';
import config from '../config.json';

const defaults = {
  locale: 'en',
  location: config.apiEndpoint,
}

const PhotoService = class {

  constructor(options = defaults) {
    this.token = options.token;
    this.location = options.location;
    this.locale = options.locale;
    this.contentType = 'application/json; charset=utf-8';

    this.sizes = [
      (config.brickWidth)
    , (config.brickWidth * 2 + config.gutter)
    , (config.brickWidth * 3 + config.gutter * 2)
    , (config.brickWidth * 4 + config.gutter * 3)
    ]
  }

  getRandomColor () {
    var letters = '0123456789ABCDEF'.split('');
    var color = '#';
    for (var i = 0; i < 6; i++ ) {
      color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
  }

  fetchPhotos (category, showHidden) {
    let me = this;
        // url = new URL('/api/v1/category/' + category + '/photo', location.origin);
        // url.searchParams.append('hidden', showHidden);

    return fetch(me.location + config.apiPrefix + '/category/' + category + '/photo', {
        headers: {
          'Authorization': me.token,
          'Accept-Language': me.locale,
          'Content-Type': me.contentType
        },
      })
      .then(response => {
        return response.text();
      })
      .then(stream => {
        return JSON.parse(stream);
      })
  }

  fetchPhoto (photoId) {
    let me = this;

    return fetch(me.location + config.apiPrefix + '/photo/' + photoId, {
        headers: {
          'Authorization': me.token,
          'Accept-Language': me.locale,
          'Content-Type': me.contentType
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

  patchPhoto(photo, props) {
    return fetch(me.location + config.apiPrefix + '/photo/' + photo.id, {
        method: 'PATCH',
        headers: {
          'Authorization': this.token,
          'Content-Type': me.contentType
        },
        body: JSON.stringify(props)
      });
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
    return fetch(me.location + config.apiPrefix + '/photo/group', {
        method: 'POST',
        headers: {
          'Authorization': this.token,
          'Content-Type': me.contentType
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  appendGroup(groupId, photos) {
    return fetch(me.location + config.apiPrefix + '/photo/group/' + groupId, {
        method: 'LINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': me.contentType
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  removeGroup(groupId, photos) {
    return fetch(me.location + config.apiPrefix + '/photo/group/' + groupId, {
        method: 'UNLINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': me.contentType
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }



  dsmap(mode, ratio, isHorisontal) {
    let
      [s1,s2,s3,s4] = this.sizes,
      modes = [
        [ratio >= 2   ? s2 : s1, s1]
      , isHorisontal ? [ratio >= 3 ? s3 : s2, s1] : [s1, s2]
      , ratio >= 4   ? [s4, s1] : [ratio >= 2 ? s3 : s2, s2]
      , isHorisontal ? [ratio >= 2 ? s4 : s3, s2] : [s2, s3]
      ];

    return modes[mode];
  }

  remapPhotos(photos) {
    let avg = photos.reduce((sum, p) => sum + p.views, 0) / photos.length;
    return photos.map(this.remapPhoto.bind(this, avg));
  }

  remapPhoto(avg, photo) {
    // console.log(arguments);
    let
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
      w: w,
      h: h,
      ratio: ratio
    })
  }

  weightedRandom = function (probabilities) {
    var probabilitiesMap = probabilities.reduce((acc, v) => {
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

    let
      exclude = photos.find(p => p.id == excludeId),

      // spread list on grouped and not grouped photos
      [init, grouped] = photos.reduce((acc, p) => {
        let [i,r] = acc;
        p.group ? r.push(p) : i.push(p);
        return acc;
      }, [[],[]]),

      groups = grouped.reduce((m,p) => {
      if (p.group) {
        let
          v = m.get(p.group) || [];

        v.push(p);
        m.set(p.group, v);
      }

      return m;
    }, new Map());

    groups.forEach((value, key) => {
        let item;

        if (exclude && exclude.group == key) {
          item = exclude;
        } else {
          item = value[Math.floor(Math.random() * value.length)];
        }

        item.views = value.reduce((sum, v) => sum + v.views, 0);
        init.push(item);
    });

    return init.sort((a,b) => a.order - b.order);
  }
};

export default PhotoService;
