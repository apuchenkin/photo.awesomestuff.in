import * as express from 'express';
import * as vary from 'vary';
import { Connection } from 'typeorm';
import { Page } from '@app/entity';
import { Language } from '@app/entity/translation';

// import PageService from '../service/page';

const router = express.Router();

router
  .get('/', async (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24 * 30}`)

    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;
    const pages = await connection
      .getRepository(Page)
      .createQueryBuilder('page')
      .leftJoinAndSelect("page.translations", "translation")
      .select([
        "page.alias",
        "translation.field",
        "translation.value",
      ])
      .where("page.hidden = :hidden", { hidden: false })
      .andWhere("translation.language = :locale", { locale })
      .andWhere("translation.field = :field", { field: "title" })
      .getMany();

    res.send(pages);
  })
  .get('/:alias', async (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24 * 30}`)
    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;

    const page = await connection
      .getRepository(Page)
      .createQueryBuilder('page')
      .leftJoinAndMapMany("page.languages", "page.translations", "lang")
      .leftJoinAndSelect("page.translations", "translation")
      .select([
        "page.alias",
        "lang.field",
        "lang.language",
        "translation.field",
        "translation.value",
      ])
      .where('page.alias = :alias', { alias: req.params.alias })
      .andWhere("page.hidden = :hidden", { hidden: false })
      .andWhere("translation.language = :locale", { locale })
      .getOne();

    if (!page) {
      res.sendStatus(404);
    }

    res.send(page);
  });

export default router;
