import express from 'express';
import expressValidator from 'express-validator';
import path from 'path';
import fs from 'fs';
import url from 'url';
import sharp from 'sharp';
import { adjust } from 'photo';
import memoize from 'memoizee';

import config from 'etc/config.json';

const app = express();
const basePath = path.join(__dirname, '..', 'static');
const adjust$ = memoize(adjust);

console.log(sharp.cache(config.cache));
console.log(sharp.concurrency());
console.log(sharp.simd(true));

app.use(express.static(basePath));

app.use(expressValidator({
  customValidators: {
    isUrlPathQuery(value) {
      if (!value) {
        return false;
      }
      const u = url.parse(value);
      if (u.protocol || u.host || !u.path) {
        return false;
      }
      return true;
    },
  },
}));

app.get('/rt?/:width/:height/*', (req, res, next) => {
  const thumb = req.url.split('/').filter(x => !!x)[0] === 'rt';

  req.checkParams('height').isInt();
  req.checkParams('width').isInt();
  req.checkParams('0').isUrlPathQuery();

  const errors = req.validationErrors();
  if (errors) {
    return res.status(400).json(errors);
  }

  if (req.fresh) {
    return res.sendStatus(304);
  }

  const
    [width, height] = adjust$(
      parseInt(req.params.width, 10),
      parseInt(req.params.height, 10)
    ),
    conf = thumb ? (config.thumb && config.thumb.opts) : config.opts,
    opts = conf && {
      kernel: sharp.kernel[conf.kernel],
      interpolator: sharp.interpolator[conf.interpolator],
    },
    filename = path.join(basePath, req.params[0]);

  fs.stat(filename, (err) => {
    if (err) {
      return res.status(404).send(err);
    }

    res.status(200);
    res.type(thumb ? 'webp' : 'jpg');

    if (!thumb) {
      res.set({
        'Content-Disposition': `inline; filename=${path.basename(req.params[0])}`,
      });
    }

    const trans = sharp(filename)
      .on('error', (sharpErr) => {
        res.status(500).send(sharpErr);
        next(new Error(err));
      })
      .resize(width, height, opts)
      .withoutEnlargement()
      .flatten()
      .normalize()
    ;

    const trans$ = thumb ? trans.min().webp() : trans.max().withMetadata();

    return trans$.pipe(res);
  });

  return res;
});

const PORT = process.env.PORT || 3002;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
