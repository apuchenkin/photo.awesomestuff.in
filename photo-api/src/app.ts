import * as express from 'express';

import { pageRouter, categoryRouter, photoRouter } from './handler';
import { locale, auth } from './middleware';

const app = express();
const router = express.Router();

router.use(auth);
router.use(locale);
router.use('/page', pageRouter);
router.use('/category', categoryRouter);
router.use('/photo', photoRouter);

app.use(router);

export default app;

