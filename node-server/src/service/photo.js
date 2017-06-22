import Photo from '../model/photo';
import Translation from '../model/translation';
import { joinTranslation } from './translation';

export const PUBLIC_FIELDS = ['id', 'src', 'views', 'width', 'height', 'group', 'datetime'];

const toPublic = language => (photo) => {
  if (!photo) {
    return null;
  }
  const result = joinTranslation(language, photo.toJSON());
  delete result.PhotoCategory;

  return result;
};

const findAll = (category, authorized, language) => category.getPhotos({
  attributes: authorized ? undefined : PUBLIC_FIELDS,
  where: authorized ? {} : {
    hidden: false,
  },
  include: [
    Object.assign({
      model: Translation,
    }, authorized ? {} : { where: { language } }),
  ],
});

const getById = (id, authorized, language) => Photo.findById(id, {
  attributes: authorized ? undefined : PUBLIC_FIELDS,
  where: authorized ? { } : {
    hidden: false,
  },
  include: [
    Object.assign({
      model: Translation,
    }, authorized ? {} : { where: { language } }),
  ],
});

export default {
  findAll,
  toPublic,
  getById,
};
