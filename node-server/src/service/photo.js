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

const findAll = (category, authorized, language) => category
  .getPhotos({ scope: ['translations', authorized ? null : { method: ['public', language] }] });

const getById = (id, authorized, language) => Photo
  .scope('translations', authorized ? null : { method: ['public', language] })
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
  toPublic,
  getById,
};
