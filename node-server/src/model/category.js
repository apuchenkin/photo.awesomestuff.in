import Sequelize from 'sequelize';
import db from '../db';
import Translation, { TYPE_CATEGORY } from './translation';

const Category = db.define('category', {
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
  image: {
    type: Sequelize.STRING,
  },
  date: {
    type: Sequelize.DATEONLY,
  },
});

Category.belongsTo(Category, { as: 'parent' });
Category.hasMany(Translation, {
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
