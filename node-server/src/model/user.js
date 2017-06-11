import Sequelize from 'sequelize';
import db from '../db';

const User = db.define('user', {
  email: {
    type: Sequelize.STRING,
    unique: true,
    validate: {
      isEmail: true,
    },
  },
  password: {
    type: Sequelize.STRING,
  },
});

export default User;
