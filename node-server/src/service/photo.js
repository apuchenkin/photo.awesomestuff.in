import Photo from '../model/photo';
import Translation from '../model/translation';
import { joinTranslation } from './translation';

const toPublic = language => (photo) => {
  if (!photo) {
    return null;
  }
  const result = joinTranslation(language, photo.toJSON());
  delete result.PhotoCategory;

  return result;
};

const LIMIT = 50;

const findAll = (category, authorized, language, page) => category
  .getPhotos({
    scope: ['translations', authorized ? null : { method: ['public', language] }].filter(Boolean),
    order: [
      ['datetime', 'ASC'],
    ],
    limit: page ? LIMIT : null,
    offset: page && (page - 1) * LIMIT,
  });

const countAll = category => category
  .countPhotos();

const getById = (id, authorized, language) => Photo
  .scope(['translations', authorized ? null : { method: ['public', language] }].filter(Boolean))
  .findById(id, {
    include: [
      {
        model: Translation,
        as: 'langs',
      },
    ],
  });

export default {
  findAll,
  countAll,
  toPublic,
  getById,
};
