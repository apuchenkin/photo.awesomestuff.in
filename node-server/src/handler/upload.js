import body from 'koa-body';
import Router from 'koa-router';
import fs from 'fs';
import path from 'path';
import mkdirp from 'mkdirp';
import { ExifImage } from 'exif/lib/exif/ExifImage';
import sizeOf from 'image-size';
import config from '../../etc/config';

import Category from '../model/category';
import Photo from '../model/photo';

const mkdir = fullName => new Promise((resolve, reject) => {
  mkdirp(path.dirname(fullName), (err) => {
    if (err) {
      reject(err);
    }
    resolve();
  });
});

const readBody = async (inputStream) => {
  const data = [];

  const promise = await new Promise((resolve, reject) => {
    inputStream.on('error', reject);
    inputStream.on('data', (chunk) => {
      data.push(chunk);
    });
    inputStream.on('end', () => {
      resolve(Buffer.concat(data));
    });
  });

  return promise;
};

const readExif = buffer => new Promise((resolve, reject) => {
  try {
    ExifImage(buffer, (error, exifData) => (
      error
        ? reject(error.message)
        : resolve(exifData)
    ));
  } catch (error) {
    reject(error.message);
  }
});

const parseDate = (datetime) => {
  const [date, time] = datetime.split(' ');
  const [YYYY, MM, DD] = date.split(':');

  return new Date(YYYY, MM - 1, DD, ...time.split(':'));
};

const upload = async (ctx) => {
  const file = ctx.request.body.files.file;
  const src = path.join(ctx.params.category, file.name);
  const fullName = path.resolve(config.static, src);

  await mkdir(fullName);
  const reader = fs.createReadStream(file.path);
  const stream = fs.createWriteStream(fullName);
  reader.pipe(stream);

  await readBody(reader)
    .then(async (buffer) => {
      const exif = await readExif(buffer);
      const { width, height } = sizeOf(buffer);
      const datetime = parseDate(exif.exif.CreateDate);
      const category = await Category.findOne({
        where: { name: ctx.params.category },
      });

      const photoData = {
        name: file.name,
        width,
        height,
        datetime,
        src,
        exif: JSON.stringify(exif),
      };

      await ctx.db.transaction(async (transaction) => {
        const photo = await Photo.create(photoData, {
          validate: true,
          transaction,
        });

        await photo.addCategory(category, { transaction });
      });
    })
    .catch(ctx.onerror);

  ctx.body = null;
};


const uploadRouter = Router();
uploadRouter.use(body({
  multipart: true,
}));
uploadRouter.post('/:category', upload);

export default uploadRouter;
