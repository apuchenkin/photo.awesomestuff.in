const env = process.env.NODE_ENV || 'development';
const config = require('./config.json') || {};
const db = require('./db.json')[env];

module.exports = Object.assign(config, {
  static: process.env.STATIC_DIR || config.static,
  database: Object.assign(db, {
    database: process.env.DB_DATABASE || db.database,
    username: process.env.DB_USER || db.username,
    password: process.env.DB_PASSWORD || db.password,
    host: process.env.DB_HOST || db.host,
    port: process.env.DB_PORT || db.port,
  }),
});
