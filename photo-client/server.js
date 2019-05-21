const proxy = require('http-proxy-middleware');
const express = require('express');
const app = require('./dist/server.bundle');

const server = express();

const PORT = process.env.PORT || 3000;

console.log('Start up dev server');

server.use('/static', express.static('./static', {
  fallthrough: false,
}));
server.use('/assets', express.static('./dist'));

if (process.env.NODE_ENV !== 'production') {
  server.use('/api/v1', proxy({
    pathRewrite: {
      '^/api/v1': '/',
    },
    target: 'http://localhost:3000',
    changeOrigin: true,
  }));
}

server.use('/', app.default());

server.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});