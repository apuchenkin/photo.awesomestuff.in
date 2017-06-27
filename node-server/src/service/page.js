import Page from '../model/page';
import Translation from '../model/translation';
import { joinTranslation } from './translation';

export const PUBLIC_FIELDS = ['alias'];

const toPublic = language => (page) => {
  if (!page) {
    return null;
  }
  console.log(page.toJSON());
  return joinTranslation(language, page.toJSON());
};

const findAll = (authorized, language) => Page.findAll({
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

const getByAlias = (alias, authorized, language) => Page.findOne({
  attributes: authorized ? undefined : PUBLIC_FIELDS,
  where: authorized ? { alias } : {
    hidden: false,
    alias,
  },
  include: [
    Object.assign({
      model: Translation,
    }, authorized ? {} : { where: { language } }),
    {
      model: Translation,
      as: 'langs',
    },
  ],
});

export default {
  findAll,
  toPublic,
  getByAlias,
};
