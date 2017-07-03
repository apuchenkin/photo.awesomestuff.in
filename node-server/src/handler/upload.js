import { PassThrough } from 'stream';
import exif from 'exif-parser';
import contentDisposition from 'content-disposition';
import fs from 'fs';
import path from 'path';
import mkdirp from 'mkdirp';
import config from '../../etc/config';

import Category from '../model/category';
import Photo from '../model/photo';

const checkImage = (ctx) => {
  if (!ctx.is('image/*')) {
    ctx.throw(415, 'images only!');
  }
};

const getFilename = (ctx) => {
  const header = ctx.get('Content-Disposition');

  if (!header) {
    ctx.thow(400, '"Content-Disposition" header missed');
  }
  const result = contentDisposition.parse(header);
  const filename = result.parameters && result.parameters.filename;

  if (!filename) {
    ctx.thow(400, 'filename missed');
  }

  return filename;
};

const writeFile = (fullName, buffer) => new Promise(
  (resolve, reject) => {
    mkdirp(path.dirname(fullName), (err) => {
      if (err) {
        reject(err);
      }

      // TODO: check if file exists, no overwrite!
      fs.writeFile(fullName, buffer, (err$) => {
        if (err$) {
          reject(err);
        }

        resolve();
      });
    });
  },
);

const readBody = async (ctx, progressStream) => {
  const stream = ctx.req;
  const data = [];

  const promise = await new Promise((resolve, reject) => {
    stream.on('error', (e) => {
      ctx.onerror(e);
      reject(e);
    });
    stream.on('data', (chunk) => {
      data.push(chunk);
      progressStream.write(`${(Buffer.concat(data).length * 100) / ctx.request.length}\n`);
    });
    stream.on('end', () => {
      resolve(Buffer.concat(data));
    });
  });

  return promise;
};

const upload = (ctx) => {
  checkImage(ctx);
  const filename = getFilename(ctx);
  const stream = PassThrough();

  ctx.body = stream;
  readBody(ctx, stream)
    .then(async (buffer) => {
      const exifData = exif.create(buffer).parse();
      const src = path.join(ctx.params.category, filename);
      const fullName = path.resolve(config.static, src);
      const category = await Category.findOne({
        where: { name: ctx.params.category },
      });

      const photoData = {
        name: filename,
        width: exifData.imageSize.width,
        height: exifData.imageSize.height,
        exif: JSON.stringify(exifData),
        datetime: new Date(exifData.tags.CreateDate * 1000),
        src,
      };

      await ctx.db.transaction(async (t) => {
        await writeFile(fullName, buffer);
        const photo = await Photo.create(photoData, {
          validate: true,
          transaction: t,
        });

        await photo.addCategory(category, { transaction: t });
      });

      stream.end('ok');
    })
    .catch((error) => {
      stream.end(`error: ${error}`);
    });
};

export default upload;
