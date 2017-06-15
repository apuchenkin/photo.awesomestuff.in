import Category from '../model/category';
import Translation from '../model/translation';

export const withTranslation = (options, language) => Object.assign(options, {
  include: [Object.assign({
    model: Translation,
  }, language
  ? ({ where: { language } }) : {})],
});

const findAll = language => Category.findAll(withTranslation({}, language));

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
