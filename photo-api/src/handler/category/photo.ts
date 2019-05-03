import * as express from 'express';
import * as vary from 'vary';
import { Connection } from 'typeorm';
import { Photo } from '@app/entity';

const photoRouter = express.Router({ mergeParams: true });

photoRouter
  .get('/', async (req, res) => {
    vary(res, 'Accept-Language');
    res.set('Cache-Control', `public, max-age=${60 * 60 * 24}`);

    const connection: Connection = req.app.locals.connection;

    const photos = await connection
      .getRepository(Photo)
      .createQueryBuilder('photo')
      .innerJoinAndSelect("photo.categories", "category", "category.name = :name", { name: req.params.category })
      .select([
        "photo.id",
        "photo.src",
        "photo.views",
        "photo.width",
        "photo.height",
        "photo.datetime",
        "photo.group",
      ])
      .where("photo.hidden = :hidden", { hidden: false })
      .andWhere("category.hidden = :hidden", { hidden: false })
      .orderBy('photo.datetime')
      .getMany();

    res.send(photos);
  })
;

export default photoRouter;
