import * as express from 'express';
import { Connection } from 'typeorm';
import { Page } from '@app/entity';

const router = express.Router();

router
  .get('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;
    const pages = await connection
      .getRepository(Page)
      .createQueryBuilder('page')
      .leftJoinAndSelect("page.translations", "translation")
      .getMany();

    res.send(pages);
  })
  .get('/:alias', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    const page = await connection
      .getRepository(Page)
      .createQueryBuilder('page')
      .leftJoinAndSelect("page.translations", "translation")
      .where('page.alias = :alias', { alias: req.params.alias })
      .getOne();

    if (!page) {
      res.sendStatus(404);
    }

    res.send(page);
  });

router.use(express.json());

export default router;
