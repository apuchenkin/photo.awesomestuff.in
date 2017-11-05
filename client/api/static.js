const jsonServer = require('json-server');
// const faker = require('faker');

const server = jsonServer.create();

server.get('/rt?/:width/:height/*', (req, res) => {
  res.send('ok');
});

module.exports = server;
