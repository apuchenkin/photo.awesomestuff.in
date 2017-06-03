import express from 'express';
import expressValidator from 'express-validator';
import path from 'path';
import fs from 'fs';
import url from 'url';
import sharp from 'sharp';
import vary from 'vary';

import { adjust } from '../../lib/util/photo/memoize';

import config from '../etc/config.json';

const app = express();
const basePath = path.join(__dirname, '..', 'static');
const extensions = ['jpg', 'png', 'jpeg'];

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
      const ext = path.extname(u.path);
      if (!extensions.find(x => `.${x}` === ext)) {
        return false;
      }
      return true;
    },
  },
}));

const checkParams = (req, res, next) => {
  const pathParam = req.params[0];

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

  Object.assign(req.params, {
    width: parseInt(req.params.width, 10),
    height: parseInt(req.params.height, 10),
    thumb: req.url.split('/').filter(x => !!x)[0] === 'rt',
  });

  Object.assign(req, {
    url: path.join(basePath, pathParam),
  });

  fs.stat(req.url, (err) => {
    if (err) {
      return res.sendStatus(404);
    }

    next();
    return false;
  });

  return false;
};

const acceptWebp = (req, res, next) => {
  if (req.headers.accept
    && req.headers.accept.indexOf('image/webp') !== -1) {
    vary(res, 'Accept');
    Object.assign(req.params, {
      webp: true,
    });
  }

  next();
};

const serveImage = (req, res, next) => {
  const
    { width, height, thumb, webp } = req.params,
    [w, h] = adjust(width, height),
    conf = thumb ? (config.thumb && config.thumb.opts) : config.opts,
    opts = conf && {
      kernel: sharp.kernel[conf.kernel],
      interpolator: sharp.interpolator[conf.interpolator],
    },
    ext = path.extname(req.url),
    basename = path.basename(req.url);


  res.status(200);
  res.type(webp ? 'webp' : ext.replace('.', ''));

  if (!thumb) {
    res.set({
      'Content-Disposition': `inline; filename=${basename + webp ? '.webp' : ''}`,
    });
  }

  const trans = sharp(req.url)
    .on('error', (sharpErr) => {
      res.sendStatus(500);
      next(new Error(sharpErr));
    })
    .resize(w, h, opts)
    .withoutEnlargement()
    .flatten()
    .normalize()
  ;

  if (thumb) {
    trans.min();
  } else {
    trans.max();
  }

  if (webp) {
    trans.webp();
  }

  return trans.pipe(res);
};

app.get('/rt?/:width/:height/*', [
  checkParams,
  acceptWebp,
  serveImage,
]);

const PORT = process.env.PORT || 3002;

app.listen(PORT, () => {
  console.log(`Server listening on: ${PORT}`); // eslint-disable-line no-console

  console.log(sharp.cache(config.cache)); // eslint-disable-line no-console
  console.log(sharp.concurrency()); // eslint-disable-line no-console
  console.log(sharp.simd(true)); // eslint-disable-line no-console
});
