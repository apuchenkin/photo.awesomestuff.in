import * as express from 'express';

import { pageRouter, categoryRouter, photoRouter, adminRouter } from './handler';
import { locale } from './middleware';

const app = express();
const router = express.Router();

router.use(locale);
router.use('/page', pageRouter);
router.use('/category', categoryRouter);
router.use('/photo', photoRouter);
router.use('/admin', adminRouter);

app.use(router);

export default app;

