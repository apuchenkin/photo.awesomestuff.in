import Category from '../model/category';
import Photo from '../model/photo';
import Translation from '../model/translation';

export const PUBLIC_FIELDS = ['id', 'name', 'date'];

export const withTranslation = (options, language) => Object.assign(options, {
  include: [Object.assign({
    model: Translation,
  }, language
  ? ({ where: { language } }) : {})],
});

const toPublic = language => (entity) => {
  if (!entity) {
    return null;
  }

  const data = entity.toJSON();
  const result = data.translations
    .filter(t => t.language === language)
    .reduce((acc, t) => Object.assign(acc, { [t.field]: t.value }), data);

  delete result.translations;
  return result;
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
      attributes: authorized ? undefined : undefined, //TODO: enrich with photo publics
    },
    Object.assign({
      model: Translation,
    }, authorized ? {} : { where: { language } }),
  ],
});

const getByName = (name, authorized, language) => Category.findOne(
  withTranslation({
    attributes: authorized ? undefined : PUBLIC_FIELDS,
    where: authorized ? { name } : {
      hidden: false,
      name,
    },
  }, authorized ? null : language))
  .then(category => (authorized ? category : toPublic(language)(category)))
;

export default {
  findAll,
  toPublic,
  getByName,
};
