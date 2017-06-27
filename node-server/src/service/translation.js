export const joinTranslation = (language, entity) => {
  const result = entity.translations
    .filter(t => t.language === language)
    .reduce((acc, t) => Object.assign(acc, {
      [t.field]: t.value,
    }), entity);

  if (entity.langs) {
    result.langs = entity.langs.map(t => t.language)
      .filter((elem, pos, arr) => arr.indexOf(elem) === pos); // drop duplicates
  }

  delete result.translations;

  return entity;
};

export default {
  joinTranslation,
};
