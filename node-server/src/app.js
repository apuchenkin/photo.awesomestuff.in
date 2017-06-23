import Koa from 'koa';
import Router from 'koa-router';
import body from 'koa-body';
import cors from 'koa-cors';
import compress from 'koa-compress';
import cacheControl from 'koa-cache-control';
import acceptLanguage from 'accept-language';
import auth from 'basic-auth';
import { ValidationError } from 'sequelize';

import db from './db';

import User from './model/user';
import Category from './model/category';

import categoryRouter from './handler/category';
import photoRouter from './handler/photo';
import translationRouter from './handler/translation';

import { LANG_RU, LANG_EN } from './model/translation';

const app = new Koa();
const router = Router();

acceptLanguage.languages(['en-US', 'ru-RU']);

router.use(body({
  urlencoded: false,
  text: false,
}));

router.use((ctx, next) => {
  const locale = acceptLanguage.get(ctx.get('accept-language'));
  ctx.locale = {
    'en-US': LANG_EN,
    'ru-RU': LANG_RU,
  }[locale] || LANG_EN;

  return next();
});

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
  if (!(ctx.method === 'GET' || ctx.method === 'HEAD' || ctx.method === 'OPTIONS')) {
    if (!ctx.user) {
      ctx.throw(401);
    }
  }

  await next();
});

router.use(async (ctx, next) => {
  try {
    await next();
  } catch (err) {
    if (err instanceof ValidationError) {
      ctx.throw(400, err, ...err.errors);
    }

    throw err;
  }
});

// router.use(authorRouter.routes(), authorRouter.allowedMethods());
router.use('/category', categoryRouter.routes(), categoryRouter.allowedMethods());
router.use('/photo', photoRouter.routes(), photoRouter.allowedMethods());
router.use('/translation', translationRouter.routes(), translationRouter.allowedMethods());

app.use(cors());
app.use(cacheControl());
app.use(compress());

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

  // eslint-disable-next-line no-console
  console.log(`${ctx.method} ${ctx.url} - ${ms}`);
});

// db.sync({ force: true })
//   .then(() => User.create({
//     email: 'apuchenkin@gmail.com',
//     password: 'root',
//   }))
//   .then(() => Category.create({
//     name: 'test',
//   }).then((category) => {
//     category.createTranslation({
//       field: 'title',
//       value: 'RUTEST',
//       language: LANG_RU,
//     });
//     category.createTranslation({
//       field: 'title',
//       value: 'ENTEST',
//       language: LANG_EN,
//     });
//   }));

app.context.db = db;
app.use(router.routes(), router.allowedMethods());
app.listen(3000);
