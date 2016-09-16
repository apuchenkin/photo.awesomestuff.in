import webpack from 'webpack';
import ExtractTextPlugin from 'extract-text-webpack-plugin';
import AssetsPlugin from 'assets-webpack-plugin';
import path from 'path';

import base from './webpack.config.base.babel';

const DEBUG = base.debug;
const GLOBALS = {
  'process.env.NODE_ENV': DEBUG ? '"development"' : '"production"',
  __DEV__: DEBUG,
};

base.module.loaders[0].query.plugins.push([
  "babel-plugin-transform-require-ignore",
  {
    "extensions": [".less", ".sass", "css"]
  }
]);

module.exports = Object.assign(base, {
  entry: './server.js',

  output: {
    path: path.resolve(__dirname, '../dist'),
    filename: 'server.js',
    chunkFilename: 'server.[name].js',
    libraryTarget: 'commonjs2',
  },

  target: 'node',

  // externals: [
  //   /^\.\/assets$/,
  //   /^[@a-z][a-z\/\.\-0-9]*$/i,
  // ],

  plugins: [

    // Define free variables
    // https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new webpack.DefinePlugin({ ...GLOBALS, 'process.env.BROWSER': false }),

    // Adds a banner to the top of each generated chunk
    // https://webpack.github.io/docs/list-of-plugins.html#bannerplugin
    new webpack.BannerPlugin('require("source-map-support").install();',
      { raw: true, entryOnly: false }),

    new ExtractTextPlugin('bundle.css', {
      allChunks: true
    }),
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
  // node: {
  //   global: true,
  //   process: false,
  //   Buffer: false,
  //   __filename: false,
  //   __dirname: false,
  //   setImmediate: false
  // },
