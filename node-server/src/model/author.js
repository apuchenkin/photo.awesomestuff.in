import Sequelize from 'sequelize';
import db from '../db';

const Author = db.define('author', {
  name: {
    type: Sequelize.STRING,
    allowNull: false,
    unique: true,
  },
});

// force: true will drop the table if it already exists

export default Author;
