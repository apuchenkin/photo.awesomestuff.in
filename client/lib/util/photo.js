import resolutions from './resolution.json';

export const adjust = (w, h) => {
  const
    norms = resolutions.map(([w$, h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
    min = Math.min(...norms),
    idx = norms.findIndex(n => n === min)
    ;

  return resolutions[idx];
};

export const weightedRandom = (probabilities) => {
  const probabilitiesMap = probabilities.reduce((acc, v) => {
      acc.push(v + (acc.length ? acc[acc.length - 1] : 0));
      return acc;
    }, []),
    pointer = Math.floor(Math.random() * probabilitiesMap[probabilitiesMap.length - 1]);

  return probabilitiesMap.reduce((acc, v) => (pointer <= v ? acc : acc + 1), 0);
};

export const getSrc = (src, w, h, thumb = false) =>
  [thumb ? 'rt' : 'r', w, h, src.replace('\\', '\\\\')].join('/');

export const refinePhotos = (photos, excludeId) => {
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
};

const sizes = (width, gutter) => [
  (width)
, ((width * 2) + (gutter))
, ((width * 3) + (gutter * 2))
, ((width * 4) + (gutter * 3)),
];

const dsmap = (mode, ratio, isHorisontal, config) => {
  const
    { width, gutter } = config,
    [s1, s2, s3, s4] = sizes(width, gutter),
    modes = [
      [ratio >= 2 ? s2 : s1, s1],
      isHorisontal ? [ratio >= 3 ? s3 : s2, s1] : [s1, s2],
      ratio >= 4 ? [s4, s1] : [ratio >= 2 ? s3 : s2, s2],
      isHorisontal ? [ratio >= 2 ? s4 : s3, s2] : [s2, s3],
    ];

  return modes[mode];
};

const remapPhoto = (avg, photo, config) => {
  const
    v = photo.views,
    std = Math.sqrt(Math.pow((v - avg), 2)),
    norm = [16, 8, 4, 1].map(i => i * (Math.floor(avg) + 1)),
    norm$ = [1, 2, 3, 4].map(i => Math.floor((i * std * v) / avg) + 1),
    probs = norm.map((n, i) => n + norm$[i]),
    mode = weightedRandom(probs),
    isHorisontal = (photo.width > photo.height),
    ratio = photo.width / photo.height,
    [w, h] = dsmap(mode, ratio, isHorisontal, config)
    ;

  return Object.assign(photo, {
    ratio,
    w,
    h,
  });
};

export const remapPhotos = config => (photos) => {
  const avg = photos.reduce((sum, p) => sum + p.views, 0) / photos.length;
  return photos.map(p => remapPhoto(avg, p, config));
};
