import * as express from 'express';

import { pageRouter } from './handler';
import { locale, auth } from './middleware';

// import categoryRouter from './handler/category';
// import photoRouter from './handler/photo';
// import translationRouter from './handler/translation';
// import uploadRouter from './handler/upload';
// import config from '../etc/config';

const app = express();
const router = express.Router();

// router.use(body({
//   urlencoded: false,
//   text: false,
// }));

// router.use(async (ctx, next) => {
//   try {
//     await next();
//   } catch (err) {
//     if (err instanceof ValidationError) {
//       ctx.throw(400, err, ...err.errors);
//     }
//     throw err;
//   }
// });

router.use(auth);
router.use(locale);
router.use('/page', pageRouter);
// router.use('/category', categoryRouter.routes(), categoryRouter.allowedMethods());
// router.use('/photo', photoRouter.routes(), photoRouter.allowedMethods());
// router.use('/translation', translationRouter.routes(), translationRouter.allowedMethods());
// router.use('/upload', uploadRouter.routes(), uploadRouter.allowedMethods());

// if (config.cors) {
//   app.use(cors());
// }

// app.use(cacheControl());
// app.use(compress());

app.use(router);

export default app;

