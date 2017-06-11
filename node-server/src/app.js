import Koa from 'koa';
import Router from 'koa-router';
import acceptLanguage from 'accept-language';
import db from './db';
import User from './model/user';
import Category from './model/category';
import authorRouter from './handler/author';
import categoryRouter from './handler/category';
import photoRouter from './handler/photo';
import { LANG_RU, LANG_EN } from './model/translation';

const app = new Koa();
const router = Router();

acceptLanguage.languages(['en-US', 'ru-RU']);

router.get('/', async (ctx) => {
  try {
    await ctx.db.authenticate();
    const users = await User.findAll();
    ctx.body = users;
  } catch (err) {
    ctx.body = 'Unable to connect to the database:';
    console.error(err);
  }
});

router.use('/author', authorRouter.routes(), authorRouter.allowedMethods());
router.use('/category', categoryRouter.routes(), categoryRouter.allowedMethods());
router.use('/photo', photoRouter.routes(), photoRouter.allowedMethods());

app
  .use((ctx, next) => {
    const locale = acceptLanguage.get(ctx.get('accept-language'));
    ctx.locale = {
      'en-US': LANG_EN,
      'ru-RU': LANG_RU,
    }[locale] || LANG_EN;

    return next();
  })
  .use(router.routes(), router.allowedMethods());

db.sync({ force: true })
  .then(() => User.create({
    email: 'apuchenkin@gmail.com',
    password: 'root',
  }))
  .then(() => Category.create({
    name: 'test',
  }).then((category) => {
    category.createTranslation({
      field: 'title',
      value: 'RUTEST',
      language: LANG_RU,
    });
    category.createTranslation({
      field: 'title',
      value: 'ENTEST',
      language: LANG_EN,
    });
  }));

app.context.db = db;

// x-response-time
app.use(async (ctx, next) => {
  const start = new Date();
  await next();
  const ms = new Date() - start;
  ctx.set('X-Response-Time', `${ms}ms`);
});

// logger
app.use(async (ctx, next) => {
  const start = new Date();
  await next();
  const ms = new Date() - start;

  console.log(`${ctx.method} ${ctx.url} - ${ms}`);
});

app.listen(3000);
