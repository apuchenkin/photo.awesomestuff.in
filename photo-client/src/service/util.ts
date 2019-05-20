export const weightedRandom = (probabilities: number[]) => {
  const probabilitiesMap = probabilities.reduce((acc, v) => [
    ...acc,
    v + (acc.length ? acc[acc.length - 1] : 0),
  ], []);
  const pointer = Math.floor(Math.random() * probabilitiesMap[probabilitiesMap.length - 1]);

  return probabilitiesMap.reduce((acc, v) => (pointer <= v ? acc : acc + 1), 0);
};

export const refinePhotos = (excludeId: number) => (photos: Photo[]) => {
  const exclude = photos.find(p => p.id === excludeId);
  const photos$ = photos.map((photo, key) => ({
    ...photo,
    order: key,
  }));

  const emptyMap: { [key: string]: Photo[] } = {};
  const groups = photos$.reduce((map, photo) => {
    const key = String(photo.group || 'null');

   return {
      ...map,
      [key]: [...(map[key] || []), photo],
    }
  }, emptyMap);

  const init = groups.null;
  Object.keys(groups)
    .filter(key => key !== 'null')
    .forEach(key => {
      const pool = groups[key] || [];
      const photo = (exclude && exclude.group === Number(key))
        ? exclude
        : pool[Math.floor(Math.random() * pool.length)]

      photo.views = Math.floor(pool.reduce((sum, v) => sum + v.views, 0) / pool.length);
      init.push(photo);
    })

  return init.sort((a, b) => a.id - b.id);
};

type Mode = 1 | 2 | 3 | 4;

const dsmap = (mode: Mode, ratio: number) => [
  [ratio >= 2 ? 2 : 1, 1],
  ratio > 1 ? [ratio >= 3 ? 3 : 2, 1] : [1, 2],
  ratio >= 4 ? [4, 1] : [ratio >= 2 ? 3 : 2, 2],
  ratio > 1 ? [ratio >= 2 ? 4 : 3, 2] : [2, 3],
][mode];

const remapPhoto = (avg: number, photo: Photo) => {
  const
    v = photo.views,
    std = Math.sqrt(Math.pow((v - avg), 2)),
    norm = [16, 8, 4, 1].map(i => i * (Math.floor(avg) + 1)),
    norm$ = [1, 2, 3, 4].map(i => Math.floor((i * std * v) / (avg + 1)) + 1),
    probs = norm.map((n, i) => n + norm$[i]),
    mode = weightedRandom(probs) as Mode,
    ratio = photo.width / photo.height,
    [w, h] = dsmap(mode, ratio)
  ;

  return {
    ratio,
    w,
    h,
  };
};

export const remapPhotos = (photos: Photo[]) => {
  const avg = photos.reduce((sum, p) => sum + p.views, 0) / photos.length;
  return photos.reduce((map, photo) => ({
    ...map,
    [photo.id]: remapPhoto(avg, photo),
  }), {});
};