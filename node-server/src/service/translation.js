export const joinTranslation = (language, entity) => {
  const result = entity.translations
    .filter(t => t.language === language)
    .reduce((acc, t) => Object.assign(acc, {
      [t.field]: t.value,
    }), entity);

  delete result.translations;

  return entity;
};

export default {
  joinTranslation,
};
