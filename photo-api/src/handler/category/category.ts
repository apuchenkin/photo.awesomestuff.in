import * as express from 'express';
import * as vary from 'vary';
import { Connection } from 'typeorm';
import { Category } from '@app/entity';
import { Language } from '@app/entity/translation';
import { unnest, categoryDTO } from '@app/dto/category';
import photoRouter from './photo';

const categoryRouter = express.Router({ mergeParams: true });
categoryRouter
  .use('/photo', photoRouter)
  .get('/', async (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24 * 30}`);

    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;

    const category = await connection
      .getRepository(Category)
      .createQueryBuilder('category')
      .leftJoinAndSelect("category.translations", "translation", "translation.language = :locale", { locale })
      .leftJoinAndSelect("category.children", "children")
      .leftJoinAndSelect("category.featured", "featured")
      .leftJoinAndSelect("children.translations", "childrenTranslation", "childrenTranslation.language = :locale", { locale })
      .select([
        "category.name",
        "category.date",
        "translation.field",
        "translation.value",
        "children.name",
        "children.date",
        "childrenTranslation.field",
        "childrenTranslation.value",
        "featured.src",
      ])
      .where('category.name = :name', { name: req.params.category })
      .andWhere("category.hidden = :hidden", { hidden: false })
      .getOne();

    if (!category) {
      res.sendStatus(404);
    } else {
      res.send(categoryDTO(category))
    }
  })
;

const categoriesRouter = express.Router();
categoriesRouter
  .use('/:category', categoryRouter)
  .get('/', async (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24 * 30}`);

    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;

    const categories = await connection
      .getRepository(Category)
      .createQueryBuilder('category')
      .leftJoinAndSelect("category.translations", "translation",
        "translation.language = :locale AND translation.field = :field", { locale, field: "title" })
      .leftJoinAndSelect("category.featured", "featured")
      .leftJoinAndSelect("category.parent", "parent")
      .select([
        "category.name",
        "category.date",
        "translation.field",
        "translation.value",
        "parent.name",
        "featured.src",
      ])
      .where("category.hidden = :hidden", { hidden: false })
      .getMany();

    res.send(unnest(categories).map(categoryDTO));
  })
;

export default categoriesRouter;
