import * as express from 'express';
import { auth } from '@app/middleware';
import * as bodyParser from 'body-parser';
import page from './page';
import category from './category';

const router = express.Router();

// router.use(auth);
router.use(bodyParser.json());
router.use('/page', page);
router.use('/category', category);

export default router;