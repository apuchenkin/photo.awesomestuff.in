import Router from 'koa-router';
import body from 'koa-body';

import Author from '../model/author';

const router = Router({ prefix: '/author' });
router
  .get('/', async (ctx) => {
    ctx.body = await Author.findAll();
  })
  .use(body())
  .post('/', async (ctx) => {
    const author = await Author.create(ctx.request.body, { validate: true });
    ctx.body = author;
  });

export default router;
