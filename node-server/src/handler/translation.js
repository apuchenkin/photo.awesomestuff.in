import Router from 'koa-router';
import queryString from 'query-string';

import Translation from '../model/translation';

const translationRouter = Router();

translationRouter
  .get('/', async (ctx) => {
    const query = queryString.parse(ctx.search);
    ctx.body = await Translation.findAll({
      where: query,
    });
  })
  .post('/', async (ctx) => {
    ctx.body = await Translation.create(ctx.request.body);
  })
  .patch('/:translationId', async (ctx) => {
    const translation = await Translation.findById(ctx.params.translationId);
    ctx.body = await translation.update(ctx.request.body);
  })
  .del('/:translationId', async (ctx) => {
    const translation = await Translation.findById(ctx.params.translationId);
    await translation.destroy();
    ctx.body = null;
  });

export default translationRouter;
