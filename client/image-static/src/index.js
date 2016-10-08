import express from 'express';
import expressValidator from 'express-validator';
import path from 'path';
import fs from 'fs';
import url from 'url';
import sharp from 'sharp';

const app = express();

sharp.cache();
sharp.concurrency(4);
console.log(sharp.simd(true));

var router = express.Router();

router.use(expressValidator({
  customValidators: {
    isSharpFormat: function(value) {
      return sharp.format.hasOwnProperty(value);
    },
    isUrlPathQuery: function(value) {
      if (!value) {
        return false;
      }
      var u =  url.parse(value);
      if (u.protocol || u.host || !u.path) {
        return false;
      }
      return true;
    },
  },
}));

router.get('/:width/:height/*', function(req, res, next) {

  console.log(req.params);
  req.checkParams('height').isInt();
  req.checkParams('width').isInt();
  req.checkParams('0').isUrlPathQuery();

  var errors = req.validationErrors();
  if (errors) {
   return res.status(400).json(errors);
  }

  // var imageUrl = url.parse(req.query.url);
  // imageUrl.host = options.baseHost;
  // imageUrl.protocol = imageUrl.protocol || 'http';
  // imageUrl = url.format(imageUrl);

  // var transformer = transform(width, height)
  //  .on('error', function sharpError(err) {
  //    res.status(500);
  //    next(new Error(err));
  //  });

  if (req.fresh) {
   return res.sendStatus(304);
  }

  const
    width = parseInt(req.params.width, 10),
    height = parseInt(req.params.height, 10),
    opts = {
     interpolator: sharp.interpolator.nohalo
    },
    filename = path.join(__dirname, '..', 'static', req.params[0]);

  sharp(filename)
    .on('error', function sharpError(err) {
     res.status(500).send(err);
     next(new Error(err));
    })
    .resize(width, height, opts)
    .max()
    .withoutEnlargement()
    .flatten()
    .normalize()
    .webp()
    .withMetadata()
    .pipe(res);

});

app.use(router);

// app.use('/:width/:height/:path', (req, res) => {
//   console.log(req.params);
//   const
//     filename = path.join(__dirname, '..', 'static', req.url);
//
//   fs.stat(filename, (err, stat) => {
//     if (err) {
//       res.status(404).send(err);
//     }
//
//     res.status(200);
//     res.type('webp');
//     // res.set({
//     //   'Content-Disposition': 'inline; filename=1.webp'
//     // });
//
//     sharp(filename)
//       .resize(300, 200, {
//         interpolator: 'nohalo'
//       })
//       .withoutEnlargement()
//       .flatten()
//       .normalize()
//       .webp()
//       .withMetadata()
//       // .on('info', function(info) {
//       //   console.log('Image height is ' + info.height);
//       // });
//       .pipe(res.status(200))
//   })
// });

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
