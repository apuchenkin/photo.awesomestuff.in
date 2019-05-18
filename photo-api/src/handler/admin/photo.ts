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
  .post('/group', async (req, res) => {
    const pids = req.body;
    const connection: Connection = req.app.locals.connection;
    const repository = connection.getRepository(Photo);
    const data = await repository.createQueryBuilder('photo')
      .select("MAX(photo.group)", 'max')
      .getRawOne();

    await repository.update(
      pids,
      { group: Number(data.max) + 1 },
    )

    res.sendStatus(204);
  })
  // @ts-ignore
  .link('/group/:groupId', async (req, res) => {
    const group = Number(req.params.groupId);
    const pids = req.body;
    const connection: Connection = req.app.locals.connection;
    const repository = connection.getRepository(Photo);

    await repository.update(
      pids,
      { group },
    )

    res.sendStatus(204);
  })
  // @ts-ignore
  .unlink('/group/:groupId', async (req, res) => {
    const pids = req.body;
    const connection: Connection = req.app.locals.connection;
    const repository = connection.getRepository(Photo);

    await repository.update(
      pids,
      { group: null },
    )

    res.sendStatus(204);
  })
;

export default photoRouter;
