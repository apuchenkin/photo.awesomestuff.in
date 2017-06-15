import Router from 'koa-router';
import body from 'koa-body';
import { PassThrough } from 'stream';
import exif from 'exif-parser';
import contentDisposition from 'content-disposition';
import fs from 'fs';
import path from 'path';
import mkdirp from 'mkdirp';
import Photo from '../model/photo';
import Category from '../model/category';
import { withTranslation } from '../service/category';

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

const findPhoto = (id, language) => Photo.findById(id, withTranslation({}, language));

const photoRouter = Router({ prefix: '/:photo' });
photoRouter
  .param('photo', async (category, ctx, next) => {
    ctx.photo = await findPhoto(ctx.params.photo);
    return next();
  })
  .use(body())
  .get('/', (ctx) => {
    ctx.body = ctx.photo;
  })
  .patch('/', async (ctx) => {
    ctx.body = await ctx.photo.update(ctx.request.body);
  })
  .del('/', async (ctx) => {
    await ctx.photo.destroy();
    ctx.body = null;
  })
;

const photosRouter = Router();
photosRouter
  .use(photoRouter.routes(), photoRouter.allowedMethods())
  .get('/', async (ctx) => {
    ctx.body = await Photo.findAll();
  })
  .post('/group', async (ctx) => {
    const pids = ctx.request.body;
    const maxGroup = await Photo.max('group');
    const group = (maxGroup || 0) + 1;
    await Photo.update(
      { group },
      { where: { id: { $in: pids } } },
    );

    ctx.body = group;
  })
  .link('/group/:groupId', async (ctx) => {
    const group = ctx.params.groupId;
    await Photo.update(
      { group },
      { where: { id: { $in: ctx.request.body } } },
    );
    ctx.body = null;
  })
  .unlink('/group/:groupId', async (ctx) => {
    await Photo.update(
      { group: null },
      { where: { id: { $in: ctx.request.body } } },
    );
    ctx.body = null;
  })
  .post('/:category', (ctx) => {
    checkImage(ctx);
    const filename = getFilename(ctx);
    const stream = PassThrough();

    ctx.body = stream;
    readBody(ctx, stream)
      .then(async (buffer) => {
        const exifData = exif.create(buffer).parse();
        const src = path.join(ctx.params.category, filename);
        const fullName = path.resolve(process.cwd(), '..', 'static', src);
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
  });

export default photosRouter;
