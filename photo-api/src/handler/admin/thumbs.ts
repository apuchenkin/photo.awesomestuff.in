import { join, basename, extname, dirname } from 'path';
import { ensureDir } from 'fs-extra';
import * as sharp from 'sharp';

const THUMB_FOLDER = 'thumb';

const opts = {
  kernel: sharp.kernel.lanczos3,
  withoutEnlargement: true,
  fit: sharp.fit.outside,
  position: sharp.strategy.attention,
};

const brickWidth = 100;
const gutter = 10;
const sizes = [
  brickWidth,
  brickWidth * 2 + gutter,
  brickWidth * 3 + gutter * 2,
];

export const generateThumb = async (filePath: string) => {
  const fileName = basename(filePath);

  const uploads = sizes.map(async size => {
    const ext = extname(fileName);
    const thumbName = `${basename(fileName, ext)}@${size}${ext}`;
    const thumbFolder = join('static', THUMB_FOLDER, dirname(filePath));
    await ensureDir(thumbFolder);

    const file = await sharp(join('static', 'photo', filePath))
      .resize(size, size, opts)
      .normalize()
      .toFile(join(thumbFolder, thumbName));

    console.log(file);
  });

  await Promise.all(uploads);
};