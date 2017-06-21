import Router from 'koa-router';

import Category from '../model/category';
import CategoryService, { withTranslation } from '../service/category';

const photoRouter = Router();
photoRouter
  .get('/', async (ctx) => {
    ctx.body = await ctx.category.getPhotos(withTranslation({}, null));
  })
  .link('/', async (ctx) => {
    await ctx.category.addPhotos(ctx.request.body);
    ctx.body = null;
  })
  .unlink('/', async (ctx) => {
    await ctx.category.removePhotos(ctx.request.body);
    ctx.body = null;
  })
;

const translationRouter = Router();
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

const categoryRouter = Router({ prefix: '/:category' });
categoryRouter
  .param('category', async (name, ctx, next) => {
    const category = await CategoryService.getByName(ctx.params.category, ctx.user, ctx.locale);
    if (!category) {
      ctx.throw(404);
    }
    ctx.category = category;
    await next();
  })
  .use('/photo', photoRouter.routes(), photoRouter.allowedMethods())
  .use('/translation', translationRouter.routes(), translationRouter.allowedMethods())
  .get('/', (ctx) => {
    ctx.body = ctx.category;
  })
  .patch('/', async (ctx) => {
    const data = ctx.request.body;
    const category = await ctx.category.update(ctx.request.body);
    if (data.featured) {
      await category.setFeatured(data.featured);
    }
    ctx.body = category;
  })
  .del('/', async (ctx) => {
    await ctx.category.destroy();
    ctx.body = null;
  })
;

const categoriesRouter = Router();
categoriesRouter
  .use(categoryRouter.routes(), categoryRouter.allowedMethods())
  .get('/', async (ctx) => {
    const categories = await CategoryService.findAll(ctx.user, ctx.locale);
    ctx.body = ctx.user ? categories : categories.map(CategoryService.toPublic(ctx.locale));
  })
  .post('/', async (ctx) => {
    const category = await Category.create(ctx.request.body, { validate: true });
    ctx.body = category;
  })
;

export default categoriesRouter;
