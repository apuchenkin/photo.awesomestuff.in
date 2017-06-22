import Router from 'koa-router';

import Photo from '../model/photo';
import upload from './upload';
import PhotoService from '../service/photo';

const translationRouter = Router();
translationRouter
  .get('/', async (ctx) => {
    if (!ctx.user) {
      ctx.throw(401);
    }

    ctx.body = await ctx.photo.getTranslations();
  })
  .post('/', async (ctx) => {
    ctx.body = await ctx.photo.createTranslation(ctx.request.body);
  })
  .patch('/:translationId', async (ctx) => {
    const translation = ctx.photo.translations.find(t =>
      t.id === Number(ctx.params.translationId),
    );
    ctx.body = await translation.update(ctx.request.body);
  })
  .del('/:translationId', async (ctx) => {
    const translation = ctx.photo.translations.find(t =>
      t.id === Number(ctx.params.translationId),
    );
    await translation.destroy();
    ctx.body = null;
  });

const photoRouter = Router({ prefix: '/:photo' });
photoRouter
  .param('photo', async (id, ctx, next) => {
    const photo = await PhotoService.getById(id, ctx.user, ctx.locale);
    if (!photo) {
      ctx.throw(404);
    }
    ctx.photo = photo;
    await next();
  })
  .use('/translation', translationRouter.routes(), translationRouter.allowedMethods())
  .get('/', (ctx) => {
    ctx.body = ctx.user ? ctx.photo : PhotoService.toPublic(ctx.locale)(ctx.photo);
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
  .post('/:category', upload);

export default photosRouter;
