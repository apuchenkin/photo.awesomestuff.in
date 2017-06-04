import Koa from 'koa';
import Router from 'koa-router';
import db from './db';
import User from './model/user';
import authorRouter from './handler/author';
import categoryRouter from './handler/category';
import photoRouter from './handler/photo';

const app = new Koa();
const router = Router();

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

router.use('/author', authorRouter.routes());
router.use('/author', authorRouter.allowedMethods());

router.use('/category', categoryRouter.routes());
router.use('/category', categoryRouter.allowedMethods());

router.use('/photo', photoRouter.routes());
router.use('/photo', photoRouter.allowedMethods());

app
  .use(router.routes())
  .use(router.allowedMethods());

db.sync({ force: true }).then(() => User.create({
  email: 'apuchenkin@gmail.com',
  password: 'root',
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
