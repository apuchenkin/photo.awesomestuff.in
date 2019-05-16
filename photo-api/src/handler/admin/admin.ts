import * as express from 'express';
import { auth } from '@app/middleware';
import page from './page';
import category from './category';
import upload from './upload';

const router = express.Router();

// router.use(auth);
router.use('/page', page);
router.use('/category', category);
router.use('/upload', upload);

export default router;