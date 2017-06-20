import Router from 'koa-router';
import body from 'koa-body';

import Category from '../model/category';
import CategoryService, { withTranslation } from '../service/category';

const photoRouter = Router({ prefix: '/photo' });
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

const translationRouter = Router({ prefix: '/translation' });
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

const categoriesRouter = Router({ prefix: '/category' });
categoriesRouter
  .use(body())
  .use(categoryRouter.routes(), categoryRouter.allowedMethods())
  .get('/', async (ctx) => {
    ctx.body = await CategoryService.findAll(ctx.locale, ctx.user);
  })
  .post('/', async (ctx) => {
    const category = await Category.create(ctx.request.body, { validate: true });
    ctx.body = category;
  })

;

export default categoriesRouter;
