import Router from 'koa-router';
import body from 'koa-body';

import Category from '../model/category';
import CategoryService from '../service/category';

const categoriesRouter = Router();

const categoryRouter = Router({ prefix: '/:category' });
const photoRouter = Router({ prefix: '/photo' });
const translationRouter = Router({ prefix: '/translation' });

photoRouter
  .get('/', async (ctx) => {
    ctx.body = await ctx.category.getPhotos();
  });

translationRouter
  .get('/', async (ctx) => {
    ctx.body = await ctx.category.getTranslations();
  })
  .post('/', async (ctx) => {
    ctx.body = await ctx.category.createTranslation(ctx.request.body);
  })
  .patch('/:translationId', async (ctx) => {
    const translation = ctx.category.translations.find(t =>
      t.id === Number(ctx.params.translationId),
    );
    ctx.body = await translation.update(ctx.request.body);
  })
  .del('/:translationId', async (ctx) => {
    const translation = ctx.category.translations.find(t =>
      t.id === Number(ctx.params.translationId),
    );
    await translation.destroy();
    ctx.body = null;
  });

categoryRouter
  .use(photoRouter.routes(), photoRouter.allowedMethods())
  .use(translationRouter.routes(), translationRouter.allowedMethods())
  .param('category', async (category, ctx, next) => {
    ctx.category = await CategoryService.getByName(ctx.params.category);
    return next();
  })
  .get('/', (ctx) => {
    ctx.body = ctx.category;
  })
  .patch('/', async (ctx) => {
    ctx.body = await ctx.category.update(ctx.request.body);
  })
  .del('/', async (ctx) => {
    await ctx.category.destroy();
    ctx.body = null;
  })
;

categoriesRouter
  .use(body())
  .use(categoryRouter.routes(), categoryRouter.allowedMethods())
  .get('/', async (ctx) => {
    // TODO: false represents public access
    ctx.body = await CategoryService.findAll(false ? ctx.locale : null);
  })
  .post('/', async (ctx) => {
    const category = await Category.create(ctx.request.body, { validate: true });
    ctx.body = category;
  })

;

export default categoriesRouter;
