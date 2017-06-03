import Sequelize from 'sequelize';
import db from '../db';

const Translation = db.define('translation', {
  language: {
    type: Sequelize.ENUM('ru', 'en'),
    allowNull: false,
  },
  refType: {
    type: Sequelize.ENUM('category', 'photo', 'page'),
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
}, {
  indexes: [
    // Create a unique index on poem
    {
      unique: true,
      fields: ['language', 'refType', 'refId', 'field'],
    },
  ],
});

// force: true will drop the table if it already exists
// Translation.sync({ force: true });

export default Translation;
