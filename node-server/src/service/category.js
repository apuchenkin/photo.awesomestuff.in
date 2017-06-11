import Category from '../model/category';
import Translation from '../model/translation';

const findAll = async (language) => {
  const raw = await Category.findAll({
    include: [{
      model: Translation,
      where: {
        language,
      },
    }],
  });
  // console.log(raw);
  // const categories = raw.map(category =>
  //   category.translations.reduce((acc, v) =>
  //     Object.assign(acc, { [v.field]: v.value }),
  //     category,
  //   ),
  // );

  return raw;
};

export default {
  findAll,
};
