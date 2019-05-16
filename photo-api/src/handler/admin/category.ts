import * as express from 'express';
import { Connection } from 'typeorm';
import { Category, Photo } from '@app/entity';

const photoRouter = express.Router({ mergeParams: true });

photoRouter
  .get('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    const photos = await connection
      .getRepository(Photo)
      .createQueryBuilder('photo')
      .leftJoinAndSelect("photo.translations", "translation")
      .innerJoinAndSelect("photo.categories", "category", "category.name = :name", { name: req.params.category })
      .orderBy('photo.datetime')
      .getMany();

    res.send(photos);
  })
;

const categoryRouter = express.Router({ mergeParams: true });
categoryRouter
  .use('/photo', photoRouter)
  .get('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;
    const category = await connection
      .getRepository(Category)
      .createQueryBuilder('category')
      .leftJoinAndSelect("category.translations", "translation")
      .leftJoinAndSelect("category.featured", "featured")
      .leftJoinAndSelect("category.parent", "parent")
      .leftJoinAndSelect("category.children", "children")
      .where('category.name = :name', { name: req.params.category })
      .getOne();

    if (!category) {
      res.sendStatus(404);
    } else {
      res.send(category)
    }
  })
  .patch('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    try {
      const category = await connection
        .getRepository(Category)
        .update({ name: req.params.category }, req.body)
        res.send(category);

    } catch (error) {
      res.status(400).send(error);
    }
  })
  .delete('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    try {
      await connection
        .getRepository(Category)
        .delete({ name: req.params.category })

      res.sendStatus(204);
    } catch (error) {
      res.status(400).send(error);
    }
  })
;

const categoriesRouter = express.Router();
categoriesRouter
  .use('/:category', categoryRouter)
  .get('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    const categories = await connection
      .getRepository(Category)
      .createQueryBuilder('category')
      .leftJoinAndSelect("category.translations", "translation")
      .leftJoinAndSelect("category.featured", "featured")
      .leftJoinAndSelect("category.parent", "parent")
      .leftJoinAndSelect("category.children", "children")
      .getMany();

    res.send(categories);
  })
  .post('/', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    try {
      const category = await connection
      .getRepository(Category)
      .save(req.body)

      res.send(category);
    } catch (error) {
      res.status(400).send(error);
    }
  })
;

categoriesRouter.use(express.json());

export default categoriesRouter;
