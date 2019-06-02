import * as express from 'express';
import * as path from 'path';
import * as sizeOf from 'image-size';
import * as busboy from 'connect-busboy';
import { ensureDir, writeFile } from 'fs-extra';
import { ExifImage, ExifData } from 'exif';
import { Category, Photo, PhotoTranslation } from '@app/entity';
import { Connection } from 'typeorm';
import { generateThumb } from './thumbs';

const readBody = async (inputStream): Promise<Buffer> => {
  const data = [];

  const promise = await new Promise<Buffer>((resolve, reject) => {
    inputStream.on('error', reject);
    inputStream.on('data', (chunk) => {
      data.push(chunk);
    });
    inputStream.on('end', () => {
      resolve(Buffer.concat(data));
    });
  });

  return promise;
};

const readExif = buffer => new Promise<ExifData>((resolve, reject) => {
  try {
    new ExifImage(buffer, (error, exifData) => (
      error
        ? reject(error.message)
        : resolve(exifData)
    ));
  } catch (error) {
    reject(error.message);
  }
});

const parseDate = (datetime) => {
  const [date, time] = datetime.split(' ');
  const [YYYY, MM, DD] = date.split(':');

  return new Date(YYYY, MM - 1, DD, ...time.split(':'));
};

const upload: express.RequestHandler = async (req, res) => {
  const connection: Connection = req.app.locals.connection;
  const photoRepository = connection.getRepository(Photo);
  const translationRepository = connection.getRepository(PhotoTranslation);

  const category = await connection.getRepository(Category).findOne({
    name: req.params.category,
  });

  if (req.busboy) {
    req.busboy.on('file', async (fieldname, fileStream, filename, encoding, mimetype) => {
      try {
        const src = `${req.params.category}/${filename}`;
        const fullName = path.resolve('./static/photo', src);

        const file = await readBody(fileStream);
        const exif = await readExif(file);
        const { width, height } = sizeOf(file);
        const datetime = parseDate(exif.exif.CreateDate);

        const photo = photoRepository.create({
          src,
          name: filename,
          width,
          height,
          datetime,
          // @ts-ignore
          author: exif.image.Artist,
          exif: JSON.stringify(exif),
          categories: [
            category,
          ],
        })

        await ensureDir(path.dirname(fullName));
        await writeFile(fullName, file);
        await generateThumb(src);
        await photoRepository.save(photo);

        // @ts-ignore
        if (exif.image.ImageDescription) {
          const translation = new PhotoTranslation();

          translation.field = "description";
          // @ts-ignore
          translation.value = exif.image.ImageDescription;
          translation.photo = photo;
          await translationRepository.save(translation);
        }

        res.status(200).send(photo);
      } catch (error) {
        res.status(400).send(error);
      }
    });

    req.pipe(req.busboy);
  } else {
    res.sendStatus(406);
  }
};

const uploadRouter = express.Router();
uploadRouter.use(busboy())
uploadRouter.post('/:category', upload);

export default uploadRouter;
