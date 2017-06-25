import { sequelize, Sequelize } from './index';
import Author from './author';
import Translation, { TYPE_PHOTO } from './translation';

const Photo = sequelize.define('photo', {
  name: {
    type: Sequelize.STRING,
    allowNull: false,
  },
  src: {
    type: Sequelize.STRING,
    allowNull: false,
    unique: true,
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
    defaultValue: 0,
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

Photo.hasMany(Translation, {
  foreignKey: 'refId',
  constraints: false,
  scope: {
    refType: TYPE_PHOTO,
  },
});
Translation.belongsTo(Photo, {
  foreignKey: 'refId',
  constraints: false,
  as: TYPE_PHOTO,
});

export default Photo;
