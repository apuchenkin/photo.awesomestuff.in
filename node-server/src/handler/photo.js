import Router from 'koa-router';
import body from 'koa-body';

import Photo from '../model/photo';

const router = Router();

router
  .get('/', async (ctx) => {
    ctx.body = await Photo.findAll();
  })
  // .use(body())
  .post('/', async (ctx) => {
    const promise = new Promise((resolve, reject) => {
      const stream = ctx.req;
      const data = [];
      stream.on('error', (e) => {
        console.log('error');
        reject(e);
      });
      stream.on('close', (e) => {
        console.log('close');
      });
      stream.on('data', (chunk) => {
        data.push(chunk);
        console.log('push');
      });
      stream.on('end', () => {
        const result = Buffer.concat(data).toString();
        console.log('ok');

        resolve(result);
      });
    })
    const res = await promise;

    // const category = await Photo.create(ctx.request.body, { validate: true });
    ctx.body = 'ok';
  });

export default router;
