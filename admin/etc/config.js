const config = require('./config.json') || {};

module.exports = Object.assign(config, {
  basename: process.env.BASENAME || config.basename,
  staticEndpoint: process.env.STATIC_ENDPOINT || config.staticEndpoint,
  apiEndpoint: process.env.API_ENDPOINT || config.apiEndpoint,
});
