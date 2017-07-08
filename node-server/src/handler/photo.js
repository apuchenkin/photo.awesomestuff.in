import Router from 'koa-router';
import vary from 'vary';

import Photo from '../model/photo';
import PhotoService from '../service/photo';

const translationRouter = Router();
translationRouter
  .get('/', async (ctx) => {
    ctx.assert(ctx.user, 401);
    ctx.body = await ctx.photo.getTranslations();
  })
  .post('/', async (ctx) => {
    ctx.body = await ctx.photo.createTranslation(ctx.request.body);
  })
  .patch('/:translationId', async (ctx) => {
    const translation = await ctx.photo.getTranslations({
      where: {
        id: ctx.params.translationId,
      },
    }).then(t => t[0]);
    ctx.body = await translation.update(ctx.request.body);
  })
  .del('/:translationId', async (ctx) => {
    const translation = await ctx.photo.getTranslations({
      where: {
        id: ctx.params.translationId,
      },
    }).then(t => t[0]);
    await translation.destroy();
    ctx.body = null;
  });

const photoRouter = Router({ prefix: '/:photo' });
photoRouter
  .param('photo', async (id, ctx, next) => {
    const photo = await PhotoService.getById(id, ctx.user, ctx.locale);
    ctx.assert(photo, 404);
    ctx.photo = photo;
    await next();
  })
  .use('/translation', translationRouter.routes(), translationRouter.allowedMethods())
  .get('/', async (ctx) => {
    vary(ctx.res, 'Accept-Language');
    ctx.cacheControl = {
      public: true,
      maxAge: 60 * 60 * 24,
    };
    await ctx.photo.update({ views: ctx.photo.views + 1 });
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
  });

export default photosRouter;
