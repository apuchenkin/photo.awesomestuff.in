import Router from 'koa-router';
import vary from 'vary';
import queryString from 'query-string';
import Category from '../model/category';
import CategoryService from '../service/category';
import PhotoService from '../service/photo';

const photoRouter = Router();
photoRouter
  .get('/', async (ctx) => {
    vary(ctx.res, 'Accept-Language');
    ctx.cacheControl = {
      public: true,
      maxAge: 60 * 60 * 24,
    };
    const { page } = queryString.parse(ctx.search);
    const photos = await PhotoService.findAll(ctx.category, ctx.user, ctx.locale, page);

    if (page && photos.length) {
      const total = await PhotoService.countAll(ctx.category, ctx.user, ctx.locale);
      ctx.set('X-Total-Count', total);
    }

    ctx.body = ctx.user ? photos : photos.map(PhotoService.toPublic(ctx.locale));
  })
  .link('/', async (ctx) => {
    await ctx.category.addPhotos(ctx.request.body);
    ctx.body = null;
  })
  .unlink('/', async (ctx) => {
    await ctx.category.removePhotos(ctx.request.body);
    ctx.body = null;
  });

const translationRouter = Router();
translationRouter
  .get('/', async (ctx) => {
    ctx.assert(ctx.user, 401);
    ctx.body = await ctx.category.getTranslations();
  })
  .post('/', async (ctx) => {
    ctx.body = await ctx.category.createTranslation(ctx.request.body);
  })
  .patch('/:translationId', async (ctx) => {
    const translation = await ctx.category.getTranslations({
      where: {
        id: ctx.params.translationId,
      },
    }).then(t => t[0]);
    ctx.assert(translation, 404);
    ctx.body = await translation.update(ctx.request.body);
  })
  .del('/:translationId', async (ctx) => {
    const translation = await ctx.category.getTranslations({
      where: {
        id: ctx.params.translationId,
      },
    }).then(t => t[0]);
    ctx.assert(translation, 404);
    await translation.destroy();
    ctx.body = null;
  });

const categoryRouter = Router({ prefix: '/:category' });
categoryRouter
  .param('category', async (name, ctx, next) => {
    const category = await CategoryService.getByName(ctx.params.category, ctx.user, ctx.locale);
    ctx.assert(category, 404);
    ctx.category = category;
    await next();
  })
  .use('/photo', photoRouter.routes(), photoRouter.allowedMethods())
  .use('/translation', translationRouter.routes(), translationRouter.allowedMethods())
  .get('/', (ctx) => {
    vary(ctx.res, 'Accept-Language');
    ctx.cacheControl = {
      public: true,
      maxAge: 60 * 60 * 24 * 30,
    };
    ctx.body = ctx.user ? ctx.category : CategoryService.toPublic(ctx.locale)(ctx.category);
  })
  .patch('/', async (ctx) => {
    ctx.body = await ctx.category.update(ctx.request.body);
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
    vary(ctx.res, 'Accept-Language');
    ctx.cacheControl = {
      public: true,
      maxAge: 60 * 60 * 24 * 30,
    };
    const categories = await CategoryService.findAll(ctx.user, ctx.locale);
    ctx.body = ctx.user ? categories : categories.map(CategoryService.toPublic(ctx.locale));
  })
  .post('/', async (ctx) => {
    ctx.body = await Category.create(ctx.request.body);
  })
;

export default categoriesRouter;
