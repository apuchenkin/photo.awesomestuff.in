import Sequelize from 'sequelize';

const sequelize = new Sequelize('aws-photo', 'root', 'r00t', {
  host: 'localhost',
  port: 32770,
  dialect: 'mysql',

  pool: {
    max: 5,
    min: 0,
    idle: 10000,
  },
});

export default sequelize;
