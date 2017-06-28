import Category, { PUBLIC_FIELDS as CATEGORY_FIELDS } from '../model/category';
import Photo, { PUBLIC_FIELDS as PHOTO_FIELDS } from '../model/photo';
import Translation from '../model/translation';
import { joinTranslation } from './translation';

const toPublic = language => (category) => {
  if (!category) {
    return null;
  }

  const categoryData = joinTranslation(language, category.toJSON());

  delete categoryData.parentId;
  delete categoryData.featuredId;

  return categoryData;
};

const findAll = (authorized, language) => Category
  .scope('translations', authorized ? null : { method: ['public', language] })
  .findAll({
    include: [
      {
        model: Category,
        as: 'parent',
        attributes: authorized ? undefined : CATEGORY_FIELDS,
      },
      {
        model: Photo,
        as: 'featured',
        attributes: authorized ? undefined : PHOTO_FIELDS,
      },
      {
        model: Translation,
        as: 'langs',
      },
    ],
  });

const getByName = (name, authorized, language) => Category
  .scope('translations', authorized ? null : { method: ['public', language] })
  .findOne({
    where: { name },
    include: [
      {
        model: Category,
        as: 'parent',
        attributes: authorized ? undefined : CATEGORY_FIELDS,
      },
      {
        model: Photo,
        as: 'featured',
        attributes: authorized ? undefined : PHOTO_FIELDS,
      },
      {
        model: Translation,
        as: 'langs',
      },
    ],
  });

export default {
  findAll,
  toPublic,
  getByName,
};
