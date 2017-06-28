import Page from '../model/page';
import Translation from '../model/translation';
import { joinTranslation } from './translation';

const toPublic = language => (page) => {
  if (!page) {
    return null;
  }
  return joinTranslation(language, page.toJSON());
};

const findAll = (authorized, language) => Page
  .scope('translations', authorized ? null : { method: ['public', language] })
  .findAll();

const getByAlias = (alias, authorized, language) => Page
  .scope('translations', authorized ? null : { method: ['public', language] })
  .findOne({
    where: { alias },
    include: [
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
