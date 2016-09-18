import webpack from 'webpack';
// import ExtractTextPlugin from 'extract-text-webpack-plugin';
import AssetsPlugin from 'assets-webpack-plugin';
import path from 'path';

import base from './webpack.config.base.babel';

const DEBUG = base.debug;
const GLOBALS = {
  'process.env.NODE_ENV': DEBUG ? '"development"' : '"production"',
  __DEV__: DEBUG,
};

module.exports = Object.assign(base, {

  entry: './server.js',
  target: 'node',

  output: {
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/assets/',
    filename: 'server.js',
    chunkFilename: 'server.[name].js',
    libraryTarget: 'commonjs2',
  },

  externals: [
    /^\.\/assets$/,
    /^[@a-z][a-z\/\.\-0-9]*$/i,
  ],

  plugins: [

    // Define free variables
    // https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new webpack.DefinePlugin({ ...GLOBALS, 'process.env.BROWSER': false }),

    // Adds a banner to the top of each generated chunk
    // https://webpack.github.io/docs/list-of-plugins.html#bannerplugin
    new webpack.BannerPlugin('require("source-map-support").install();',
      { raw: true, entryOnly: false }),
  ],

  node: {
    console: false,
    global: false,
    process: false,
    Buffer: false,
    __filename: false,
    __dirname: false,
  },

  devtool: 'source-map',
});
