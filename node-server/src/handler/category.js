import Router from 'koa-router';
import body from 'koa-body';

import Category from '../model/category';
import Translation, { TYPE_CATEGORY } from '../model/translation';

const categoriesRouter = Router();

const getCategoryByName = name => Category.findOne({ where: { name } });

const categoryRouter = Router({ prefix: '/:category' });
const photoRouter = Router({ prefix: '/photo' });
const translationRouter = Router({ prefix: '/translation' });

photoRouter
  .get('/', async (ctx) => {
    ctx.body = await ctx.category.getPhotos();
  });

translationRouter
  .get('/', async (ctx) => {
    const translations = await Translation.findAll({
      where: {
        refType: TYPE_CATEGORY,
        refId: ctx.category.id,
      },
    });
    ctx.body = translations;
  })
  .post('/', async (ctx) => {
    const translation = await Translation.create(Object.assign(ctx.request.body, {
      refType: TYPE_CATEGORY,
      refId: ctx.category.id,
    }));

    ctx.body = translation;
  })
  .del('/:translationId', async (ctx) => {
    const translation = await Translation.findById(ctx.params.translationId);
    if (translation.refId === ctx.category.id) {
      await translation.destroy();
    } else {
      throw new Error('Wrong translation reference');
    }

    ctx.body = null;
  });

categoryRouter
  .use(photoRouter.routes(), photoRouter.allowedMethods())
  .use(translationRouter.routes(), translationRouter.allowedMethods())
  .param('category', async (category, ctx, next) => {
    ctx.category = await getCategoryByName(ctx.params.category);
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
    ctx.body = await Category.findAll();
  })
  .post('/', async (ctx) => {
    const category = await Category.create(ctx.request.body, { validate: true });
    ctx.body = category;
  })

;

export default categoriesRouter;
