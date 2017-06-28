import { sequelize, Sequelize } from './index';

export const LANG_RU = 'ru';
export const LANG_EN = 'en';

export const TYPE_CATEGORY = 'category';
export const TYPE_PHOTO = 'photo';
export const TYPE_PAGE = 'page';

const Translation = sequelize.define('translation', {
  refType: {
    type: Sequelize.ENUM(TYPE_CATEGORY, TYPE_PHOTO, TYPE_PAGE),
    allowNull: false,
  },
  refId: {
    type: Sequelize.INTEGER,
    allowNull: false,
  },
  field: {
    type: Sequelize.STRING,
    allowNull: false,
  },
  value: {
    type: Sequelize.TEXT,
    allowNull: false,
  },
  language: {
    type: Sequelize.ENUM(LANG_RU, LANG_EN),
    allowNull: false,
  },
}, {
  indexes: [
    {
      unique: true,
      fields: ['language', 'refType', 'refId', 'field'],
    },
  ],
});

export default Translation;
