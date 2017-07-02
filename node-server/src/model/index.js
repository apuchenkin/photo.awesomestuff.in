const env = process.env.NODE_ENV || 'development';

const Sequelize = require('sequelize');
const config = require('../../etc/db.json')[env];
// const Page = require('./page');
// const basename = path.basename(module.filename);

// const db = {
//   [Page.name]: Page,
// };
const db = {};
// console.log(db);

const database = process.env.DB_DATABASE || config.database;
const username = process.env.DB_USER || config.username;
const password = process.env.DB_PASSWORD || config.password;
const host = process.env.DB_HOST || config.host;
const port = process.env.DB_PORT || config.port;

const sequelize = new Sequelize(database, username, password, Object.assign(config, {
  host,
  port,
}));

// if (config.use_env_variable) {
//   var sequelize = new Sequelize(process.env[config.use_env_variable]);
// } else {
//   var
// }
//
// console.log(123);

// fs
//   .readdirSync(__dirname)
//   .filter(file =>
//     file.indexOf('.') !== 0
//     && (file !== basename)
//     && (file.slice(-3)) === '.js',
//   )
//   .forEach((file) => {
//     console.log(file);
//     const model = sequelize.import(path.join(__dirname, file));
//     db[model.name] = model;
//   });
//

// Object.keys(db).forEach((modelName) => {
//   if (db[modelName].associate) {
//     db[modelName].associate(db);
//   }
// });
// [
//   Page,
// ].map(model => model.associate(db));


db.sequelize = sequelize;
db.Sequelize = Sequelize;

export { sequelize, Sequelize };

export default db;
