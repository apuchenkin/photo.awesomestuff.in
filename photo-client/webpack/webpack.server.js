const baseConfig = require('./webpack.config');
const nodeExternals = require('webpack-node-externals');
const path = require('path');

// Note that since this is for the server, it is important to
// set the target to node and set the libraryTarget to commonjs2
module.exports = (env = {}) => Object.assign({}, {
  target: 'node',
  entry: './src/server/app.tsx',
  externals: [
    function(context, request, callback) {
      if (/^@assets\/assets.json/.test(request)){
        return callback(null, path.resolve(__dirname, '..', request.slice(1)));
      }
      callback();
    },
    nodeExternals(),
  ],
  output: {
    filename: 'server.bundle.js',
    path: path.resolve(__dirname, '../dist/server'),
    libraryTarget: 'commonjs2',
    publicPath: '/',
  }
}, baseConfig(env));