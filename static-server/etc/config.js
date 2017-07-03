const config = require('./config.json') || {};

module.exports = {
  ...config,
  static: process.env.STATIC_DIR || config.static,
};
