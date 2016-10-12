import resolutions from 'resolution';

export const adjust = (w, h) => {
  const
    norms = resolutions.map(([w$, h$]) => Math.pow(w$ - w, 2) + Math.pow(h$ - h, 2)),
    min = Math.min(...norms),
    idx = norms.findIndex(n => n === min)
    ;

  return resolutions[idx];
};
