const config = require('./config.json') || {};

module.exports = {
  ...config,
  staticEndpoint: process.env.STATIC_ENDPOINT || config.staticEndpoint,
  apiEndpoint: process.env.API_ENDPOINT || config.apiEndpoint,
};
