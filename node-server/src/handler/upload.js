// import { PassThrough } from 'stream';
// import through from 'through';
import exif from 'exif-parser';
import body from 'koa-body';
import Router from 'koa-router';
// import contentDisposition from 'content-disposition';
import fs from 'fs';
import path from 'path';
import mkdirp from 'mkdirp';
import config from '../../etc/config';

import Category from '../model/category';
import Photo from '../model/photo';

// const checkImage = (ctx) => {
//   if (!ctx.is('image/*')) {
//     ctx.throw(415, 'images only!');
//   }
// };

// const getFilename = (ctx) => {
//   const header = ctx.get('Content-Disposition');
//
//   if (!header) {
//     ctx.thow(400, '"Content-Disposition" header missed');
//   }
//   const result = contentDisposition.parse(header);
//   const filename = result.parameters && result.parameters.filename;
//
//   if (!filename) {
//     ctx.thow(400, 'filename missed');
//   }
//
//   return filename;
// };

const mkdir = fullName => new Promise((resolve, reject) => {
  mkdirp(path.dirname(fullName), (err) => {
    if (err) {
      reject(err);
    }
    resolve();
  });
});

// const sse = (event, data) => data;// `event:${event}\ndata: ${data}\n\n`;
//
const readBody = async (inputStream) => {
  const data = [];

  const promise = await new Promise((resolve, reject) => {
    inputStream.on('error', reject);
    inputStream.on('data', (chunk) => {
      data.push(chunk);
      // outputStream.write(sse('progress', `${(Buffer.concat(data).length * 100) / length}\n`));
    });
    inputStream.on('end', () => {
      resolve(Buffer.concat(data));
    });
  });

  return promise;
};

// body

const upload = async (ctx) => {
  // checkImage(ctx);
  // const filename = getFilename(ctx);
  // const stream = through();
  //
  // ctx.req.on('close', ctx.res.end);
  // ctx.req.on('finish', ctx.res.end);
  // ctx.req.on('error', ctx.res.end);

  // console.log(ctx.request.body.fields);
  // console.log(ctx.request.body.files);

  // const data = [];

  const file = ctx.request.body.files.file;
  const src = path.join(ctx.params.category, file.name);
  const fullName = path.resolve(config.static, src);

  await mkdir(fullName);
  const reader = fs.createReadStream(file.path);
  const stream = fs.createWriteStream(fullName);
  reader.pipe(stream);

  // reader.on('data', (chunk) => {
  //   data.push(chunk);
  // });
  // reader.on('end', () => {
  //   const buffer = Buffer.concat(data);
  // });

  await readBody(reader)
    .then(async (buffer) => {
      const exifData = exif.create(buffer).parse();

      const category = await Category.findOne({
        where: { name: ctx.params.category },
      });

      const photoData = {
        name: file.name,
        width: exifData.imageSize.width,
        height: exifData.imageSize.height,
        exif: JSON.stringify(exifData),
        datetime: new Date(exifData.tags.CreateDate * 1000),
        src,
      };

      await ctx.db.transaction(async (transaction) => {
        // await writeFile(fullName, buffer);
        const photo = await Photo.create(photoData, {
          validate: true,
          transaction,
        });

        await photo.addCategory(category, { transaction });
      });

      // stream.end(sse('end', 'ok'));
    })
    .catch(ctx.onerror);

  ctx.body = null;

  // readBody(ctx.request.length)(ctx.req, stream)
  //   .then(async (buffer) => {
  //     const exifData = exif.create(buffer).parse();
  //     const src = path.join(ctx.params.category, filename);
  //     const fullName = path.resolve(config.static, src);
  //     const category = await Category.findOne({
  //       where: { name: ctx.params.category },
  //     });
  //
  //     const photoData = {
  //       name: filename,
  //       width: exifData.imageSize.width,
  //       height: exifData.imageSize.height,
  //       exif: JSON.stringify(exifData),
  //       datetime: new Date(exifData.tags.CreateDate * 1000),
  //       src,
  //     };
  //
  //     await ctx.db.transaction(async (t) => {
  //       await writeFile(fullName, buffer);
  //       const photo = await Photo.create(photoData, {
  //         validate: true,
  //         transaction: t,
  //       });
  //
  //       await photo.addCategory(category, { transaction: t });
  //     });
  //
  //     stream.end(sse('end', 'ok'));
  //   })
  //   .catch((error) => {
  //     // ctx.onerror(error);
  //     stream.end(sse('end', `error: ${error}`));
  //   });
};


const uploadRouter = Router();
uploadRouter.use(body({
  multipart: true,
}));
uploadRouter.post('/:category', upload);

export default uploadRouter;
