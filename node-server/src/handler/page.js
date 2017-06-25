import Router from 'koa-router';
import vary from 'vary';
import PageService from '../service/page';

const router = Router();
router
  .get('/', async (ctx) => {
    vary(ctx.res, 'Accept-Language');
    ctx.cacheControl = {
      public: true,
      maxAge: 60 * 60 * 24 * 30,
    };
    const pages = await PageService.findAll(ctx.user, ctx.locale);
    ctx.body = ctx.user ? pages : pages
      .map(PageService.toPublic(ctx.locale))
      .map(data => Object.assign(data, { content: undefined }));
  })
  .get('/:alias', async (ctx) => {
    vary(ctx.res, 'Accept-Language');
    ctx.cacheControl = {
      public: true,
      maxAge: 60 * 60 * 24 * 30,
    };
    const page = await PageService.getByAlias(ctx.params.alias, ctx.user, ctx.locale);
    ctx.body = ctx.user ? page : PageService.toPublic(ctx.locale)(page);
  })
;

export default router;
