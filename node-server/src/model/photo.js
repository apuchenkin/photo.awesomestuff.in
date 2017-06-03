import Sequelize from 'sequelize';

import db from '../db';
import Author from './author';
import Category from './category';

const Photo = db.define('photo', {
  name: {
    type: Sequelize.STRING,
    allowNull: false,
  },
  src: {
    type: Sequelize.STRING,
    allowNull: false,
    unique: true,
  },
  thumb: {
    type: Sequelize.STRING,
  },
  width: {
    type: Sequelize.INTEGER,
    allowNull: false,
  },
  height: {
    type: Sequelize.INTEGER,
    allowNull: false,
  },
  exif: {
    type: Sequelize.TEXT,
    allowNull: false,
  },
  views: {
    type: Sequelize.INTEGER,
    allowNull: false,
  },
  datetime: {
    type: Sequelize.DATE,
    allowNull: false,
  },
  order: {
    type: Sequelize.INTEGER,
  },
  hidden: {
    type: Sequelize.BOOLEAN,
    allowNull: false,
    defaultValue: true,
  },
  group: {
    type: Sequelize.INTEGER,
  },
});

Photo.belongsTo(Author);

Category.belongsToMany(Photo, { through: 'PhotoCategory' });
Photo.belongsToMany(Category, { through: 'PhotoCategory' });

// force: true will drop the table if it already exists
// Author.sync({ force: true }).then(() => {
//   Photo.sync({ force: true });
//   Category.sync({ force: true });
// });

export default Photo;
