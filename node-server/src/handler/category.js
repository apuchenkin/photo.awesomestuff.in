import Router from 'koa-router';
import body from 'koa-body';

import Category from '../model/category';

const router = Router();

router
  .get('/', async (ctx) => {
    ctx.body = await Category.findAll();
  })
  .use(body())
  .post('/', async (ctx) => {
    const category = await Category.create(ctx.request.body, { validate: true });
    ctx.body = category;
  });

export default router;
