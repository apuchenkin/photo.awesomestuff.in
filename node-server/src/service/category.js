import Category from '../model/category';
import Translation from '../model/translation';

const PUBLIC_FIELDS = ['id', 'name'];

export const withTranslation = (options, language) => Object.assign(options, {
  include: [Object.assign({
    model: Translation,
  }, language
  ? ({ where: { language } }) : {})],
});

const toPublic = language => (entity) => {
  const data = entity.toJSON();
  const result = data.translations
    .filter(t => t.language === language)
    .reduce((acc, t) => Object.assign(acc, { [t.field]: t.value }), data);

  delete result.translations;
  return result;
};

const findAll = (language, authorized) => Category.findAll(
  withTranslation({
    attributes: authorized ? undefined : PUBLIC_FIELDS,
    where: authorized ? {} : {
      hidden: false,
    },
  }, authorized ? null : language))
  .then(categories => (authorized
    ? categories
    : categories.map(toPublic(language))
  ));


const getByName = (name, language) => Category.findOne(
  withTranslation(
    { where: { name } },
    language,
  ));

const getPublicData = category =>
  category.translations.reduce((acc, v) =>
    Object.assign(acc, { [v.field]: v.value }),
    category,
  );

export default {
  findAll,
  getPublicData,
  getByName,
};
