const config = require('./config.json') || {};

module.exports = Object.assign(config, {
  analytics: process.env.ANALYTICS || config.analytics,
  hostname: process.env.HOSTNAME || config.hostname,
  staticEndpoint: process.env.STATIC_ENDPOINT || config.staticEndpoint,
  apiEndpoint: process.env.API_ENDPOINT || config.apiEndpoint,
});
