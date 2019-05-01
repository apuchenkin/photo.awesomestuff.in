import * as express from 'express';
import * as vary from 'vary';
import { Connection } from 'typeorm';
import { Category } from '@app/entity';
import { Language } from '@app/entity/translation';
import { unnest, categoryDTO } from '@app/dto/category';

declare global {
  namespace Express {
    interface Request {
      category?: Category
    }
  }
}

const categoryRouter = express.Router();
categoryRouter
  .get('/', (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24 * 30}`);

    res.send(categoryDTO(req.category))
  })
;

const categoriesRouter = express.Router();
categoriesRouter
  .param('category', async (req, res, next, name) => {
    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;

    const category = await connection
      .getRepository(Category)
      .createQueryBuilder('category')
      .leftJoinAndSelect("category.translations", "translation")
      .leftJoinAndSelect("category.children", "children")
      .leftJoinAndSelect("children.translations", "childrenTranslation")
      .select([
        "category.name",
        "category.date",
        "translation.field",
        "translation.value",
        "children.name",
        "children.date",
        "childrenTranslation.field",
        "childrenTranslation.value",
      ])
      .where('category.name = :name', { name })
      .andWhere("category.hidden = :hidden", { hidden: false })
      .andWhere("translation.language = :locale", { locale })
      .andWhere("childrenTranslation.language = :locale", { locale })
      .getOne();

    if (!category) {
      res.sendStatus(404);
    } else {
      req.category = category;
      next();
    }
  })
  .use('/:category', categoryRouter)
  .get('/', async (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24 * 30}`);

    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;

    const categories = await connection
      .getRepository(Category)
      .createQueryBuilder('category')
      .leftJoinAndSelect("category.translations", "translation")
      .leftJoinAndSelect("category.parent", "parent")
      .select([
        "category.name",
        "category.date",
        "translation.field",
        "translation.value",
        "parent.name",
      ])
      .where("category.hidden = :hidden", { hidden: false })
      .andWhere("translation.language = :locale", { locale })
      .andWhere("translation.field = :field", { field: "title" })
      .getMany();

    res.send(unnest(categories).map(categoryDTO));
  })
;

export default categoriesRouter;
