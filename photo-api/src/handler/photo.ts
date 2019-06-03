import * as express from 'express';
import * as vary from 'vary';
import { Connection } from 'typeorm';
import { Photo } from '@app/entity';
import { Language } from '@app/entity/translation';
import { photoDTO } from '@app/dto/photo';

const photoRouter = express.Router();

photoRouter
  .get('/:id', async (req, res) => {
    vary(res, 'Accept-Language');

    const connection: Connection = req.app.locals.connection;
    const locale: Language = req.app.locals.locale;

    const photo = await connection
      .getRepository(Photo)
      .createQueryBuilder('photo')
      .leftJoinAndSelect("photo.translations", "translation", "translation.language = :locale", { locale })
      .select([
        "photo.id",
        "photo.src",
        "photo.views",
        "photo.datetime",
        "photo.group",
        "photo.width",
        "photo.height",
        "translation.field",
        "translation.value",
      ])
      .where("photo.id = :id", { id: req.params.id })
      .andWhere("photo.hidden = :hidden", { hidden: false })
      .getOne();

    await connection.getRepository(Photo).increment({ id: photo.id }, 'views', 1);

    res.send(photoDTO(photo));
  })
;

export default photoRouter;
