import { sequelize, Sequelize } from './index';
import Translation, { TYPE_CATEGORY } from './translation';
import Photo from './photo';

export const PUBLIC_FIELDS = ['id', 'name', 'date', 'parentId', 'featuredId'];

const Category = sequelize.define('category', {
  name: {
    type: Sequelize.STRING,
    allowNull: false,
    unique: true,
  },
  hidden: {
    type: Sequelize.BOOLEAN,
    allowNull: false,
    defaultValue: true,
  },
  date: {
    type: Sequelize.DATEONLY,
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

Category.belongsTo(Photo, { as: 'featured' });

Category.belongsToMany(Photo, { through: 'PhotoCategory' });
Photo.belongsToMany(Category, { through: 'PhotoCategory' });


Category.belongsTo(Category, { as: 'parent' });
Category.hasMany(Translation, {
  foreignKey: 'refId',
  constraints: false,
  scope: {
    refType: TYPE_CATEGORY,
  },
});

Category.hasMany(Translation, {
  as: 'langs',
  foreignKey: 'refId',
  constraints: false,
  scope: {
    refType: TYPE_CATEGORY,
  },
});

Translation.belongsTo(Category, {
  foreignKey: 'refId',
  constraints: false,
  as: TYPE_CATEGORY,
});

export default Category;
