import Koa from 'koa';
import Router from 'koa-router';
import acceptLanguage from 'accept-language';
import auth from 'basic-auth';
import db from './db';
import User from './model/user';
import Category from './model/category';
import authorRouter from './handler/author';
import categoryRouter from './handler/category';
import photoRouter from './handler/photo';
import translationRouter from './handler/translation';

import { LANG_RU, LANG_EN } from './model/translation';

const app = new Koa();
const router = Router();

acceptLanguage.languages(['en-US', 'ru-RU']);

router.use(async (ctx, next) => {
  const credentials = auth(ctx);

  if (credentials) {
    const user = await User.findOne({
      where: {
        email: credentials.name,
        password: credentials.pass,
      },
    });

    ctx.user = user;
  }
  if (ctx.method !== 'GET' && !ctx.user) {
    ctx.status = 401;
  } else {
    return next();
  }
});

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

router.use(authorRouter.routes(), authorRouter.allowedMethods());
router.use(categoryRouter.routes(), categoryRouter.allowedMethods());
router.use(photoRouter.routes(), photoRouter.allowedMethods());
router.use(translationRouter.routes(), translationRouter.allowedMethods());

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
