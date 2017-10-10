const jsonServer = require('json-server');
const db = require('./db');

const server = jsonServer.create();
const router = jsonServer.router(db);
const middlewares = jsonServer.defaults();

server.use(middlewares);
server.use(router);

module.exports = server;
