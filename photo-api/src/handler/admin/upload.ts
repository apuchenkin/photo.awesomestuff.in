import * as express from 'express';
import * as fs from 'fs';
import * as path from 'path';
import * as sizeOf from 'image-size';
import * as busboy from 'connect-busboy';
import { ensureDir } from 'fs-extra';
import { ExifImage, ExifData } from 'exif';
import { Category, Photo } from '@app/entity';
import { Connection } from 'typeorm';

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

  const category = await connection.getRepository(Category).findOne({
    name: req.params.category,
  });

  if (req.busboy) {
    req.busboy.on('file', async (fieldname, fileStream, filename, encoding, mimetype) => {
      const src = path.join(req.params.category, filename);
      const fullName = path.resolve(__dirname, './static', src);

      console.log(fullName);

      await ensureDir(path.basename(fullName));

      fileStream.pipe(fs.createWriteStream(fullName));

      const file = await readBody(fileStream);
      const exif = await readExif(file);
      const { width, height } = sizeOf(file);
      const datetime = parseDate(exif.exif.CreateDate);

      const photo = await connection.getRepository(Photo).create({
        name: filename,
        width,
        height,
        datetime,
        exif: JSON.stringify(exif),
        categories: [
          category,
        ]
      })

      res.status(200).send(photo);
    });

    req.pipe(req.busboy);
  }
};

const uploadRouter = express.Router();
uploadRouter.use(busboy())
uploadRouter.post('/:category', upload);

export default uploadRouter;
