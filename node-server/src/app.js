import Koa from 'koa';
import db from './db';
import User from './model/user';
import Photo from './model/photo';
import Page from './model/page';
import Translation from './model/translation';

const app = new Koa();

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

// response
app.use(async (ctx) => {
  try {
    await ctx.db.authenticate();
    const users = await User.findAll();
    ctx.body = users;
  } catch (err) {
    ctx.body = 'Unable to connect to the database:';
    console.error(err);
  }
});

app.listen(3000);
