import Router from 'koa-router';
import body from 'koa-body';

import Category from '../model/category';
import Translation from '../model/translation';

const router = Router();

const getCategoryByName = name => Category.findOne({ where: { name } });

router
  .use(body())
  .get('/', async (ctx) => {
    ctx.body = await Category.findAll();
  })
  .get('/:id',
    // TODO: improve as single request
    async (ctx, next) => {
      if (!Number(ctx.params.id)) return next();
      ctx.body = await Category.findById(ctx.params.id);

      return null;
    })
  .get('/:name', async (ctx) => {
    ctx.body = await getCategoryByName(ctx.params.name);
  })
  .get('/:name/photo', async (ctx) => {
    const category = await getCategoryByName(ctx.params.name);
    ctx.body = await category.getPhotos();
  })
  .get('/:name/translation', async (ctx) => {
    const category = await getCategoryByName(ctx.params.name);
    const translations = await Translation.findAll({ where: { refType: 'category', refId: category.id } });
    ctx.body = translations;
  })
  .patch('/:name', async (ctx) => {
    const category = await getCategoryByName(ctx.params.name);
    ctx.body = await category.update(ctx.request.body);
  })
  .del('/:name', async (ctx) => {
    const category = await getCategoryByName(ctx.params.name);
    category.destroy();
    ctx.body = null;
  })
  .post('/', async (ctx) => {
    const category = await Category.create(ctx.request.body, { validate: true });
    ctx.body = category;
  });

export default router;
