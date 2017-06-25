import { sequelize, Sequelize } from './index';

const Author = sequelize.define('author', {
  name: {
    type: Sequelize.STRING,
    allowNull: false,
    unique: true,
  },
});

// force: true will drop the table if it already exists

export default Author;
