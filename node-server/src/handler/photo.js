import Router from 'koa-router';
import { PassThrough } from 'stream';

import Photo from '../model/photo';

const router = Router();

router
  .get('/', async (ctx) => {
    ctx.body = await Photo.findAll();
  })
  .post('/', (ctx) => {
    const stream = ctx.req;
    const resp = PassThrough();

    const data = [];
    stream.on('error', ctx.onerror);
    // stream.on('close', () => {
    //   console.log('close');
    // });
    stream.on('data', (chunk) => {
      data.push(chunk);
      resp.write(`${(Buffer.concat(data).length * 100) / ctx.request.length}\n`);
    });
    stream.on('end', () => {
      const result = Buffer.concat(data).toString();
      resp.write('done');
      resp.end();
    });

    ctx.body = resp;
  });

export default router;
