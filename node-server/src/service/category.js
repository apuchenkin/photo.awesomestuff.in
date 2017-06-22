import Category from '../model/category';
import Photo from '../model/photo';
import Translation from '../model/translation';
import { joinTranslation } from './translation';
import { PUBLIC_FIELDS as PHOTO_FIELDS } from './photo';

export const PUBLIC_FIELDS = ['id', 'name', 'date', 'parentId', 'featuredId'];

const toPublic = language => (category) => {
  if (!category) {
    return null;
  }

  const categoryData = joinTranslation(language, category.toJSON());

  delete categoryData.parentId;
  delete categoryData.featuredId;

  return categoryData;
};

const findAll = (authorized, language) => Category.findAll({
  attributes: authorized ? undefined : PUBLIC_FIELDS,
  where: authorized ? {} : {
    hidden: false,
  },
  include: [
    {
      model: Category,
      as: 'parent',
      attributes: authorized ? undefined : PUBLIC_FIELDS,
    },
    {
      model: Photo,
      as: 'featured',
      attributes: authorized ? undefined : PHOTO_FIELDS,
    },
    Object.assign({
      model: Translation,
    }, authorized ? {} : { where: { language } }),
  ],
});

const getByName = (name, authorized, language) => Category.findOne({
  attributes: authorized ? undefined : PUBLIC_FIELDS,
  where: authorized ? { name } : {
    hidden: false,
    name,
  },
  include: [
    {
      model: Category,
      as: 'parent',
      attributes: authorized ? undefined : PUBLIC_FIELDS,
    },
    {
      model: Photo,
      as: 'featured',
      attributes: authorized ? undefined : PHOTO_FIELDS,
    },
    Object.assign({
      model: Translation,
    }, authorized ? {} : { where: { language } }),
  ],
});

export default {
  findAll,
  toPublic,
  getByName,
};
