import Sequelize from 'sequelize';
import db from '../db';

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

Category.belongsTo(Category);

export default Category;
