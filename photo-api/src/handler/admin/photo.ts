import * as express from 'express';
import { Connection } from 'typeorm';
import { Photo } from '@app/entity';

const photoRouter = express.Router();

photoRouter
  .use(express.json())
  .get('/:id', async (req, res) => {
    const connection: Connection = req.app.locals.connection;

    const photo = await connection
      .getRepository(Photo)
      .createQueryBuilder('photo')
      .leftJoinAndSelect("photo.translations", "translation")
      .where("photo.id = :id", { id: req.params.id })
      .getOne();

    res.send(photo);
  })
  .put('/:id', async (req, res) => {
    const connection: Connection = req.app.locals.connection;
    const repository = connection.getRepository(Photo);

    try {
      const photo = await repository.findOne(req.params.id);

      const photo$ = await repository
        .save(repository.merge(photo, req.body))

      res.send(photo$);
    } catch (error) {
      res.status(400).send(error);
    }
  })
;

export default photoRouter;
