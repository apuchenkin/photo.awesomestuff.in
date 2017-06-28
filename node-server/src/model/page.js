import { sequelize, Sequelize } from './index';
import Translation, { TYPE_PAGE } from './translation';

export const PUBLIC_FIELDS = ['alias'];

const Page = sequelize.define('page', {
  alias: {
    type: Sequelize.STRING,
    unique: true,
  },
  hidden: {
    type: Sequelize.BOOLEAN,
    allowNull: false,
    defaultValue: true,
  },
}, {
  scopes: {
    translations: {
      include: [Translation],
    },
    public: language => ({
      attributes: PUBLIC_FIELDS,
      where: {
        hidden: false,
      },
      include: [
        {
          model: Translation,
          where: {
            language,
          },
        },
      ],
    }),
  },
});

Page.hasMany(Translation, {
  foreignKey: 'refId',
  constraints: false,
  scope: {
    refType: TYPE_PAGE,
  },
});

Page.hasMany(Translation, {
  as: 'langs',
  foreignKey: 'refId',
  constraints: false,
  scope: {
    refType: TYPE_PAGE,
  },
});

Translation.belongsTo(Page, {
  foreignKey: 'refId',
  constraints: false,
  as: TYPE_PAGE,
});

export default Page;
