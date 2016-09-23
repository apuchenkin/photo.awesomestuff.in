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

module.exports = Object.assign({}, base, {
  entry: './index.js',

  output: {
    path: path.resolve(__dirname, '../dist/assets'),
    publicPath: '/',
    filename: DEBUG ? '[name].js' : '[name].[hash].js',
    chunkFilename: DEBUG ? '[name].[id].js' : '[name].[id].[chunkhash].js',
  },

  target: 'web',

  plugins: ([
    // Define free variables
    // https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new webpack.DefinePlugin({ ...GLOBALS, 'process.env.BROWSER': true }),

    // Emit a file with assets paths
    // https://github.com/sporto/assets-webpack-plugin#options
    new AssetsPlugin({
      path: path.resolve(__dirname, '../dist'),
      filename: 'assets.json',
      // processOutput: x => `module.exports = ${JSON.stringify(x)};`,
    }),

    // Assign the module and chunk ids by occurrence count
    // Consistent ordering of modules required if using any hashing ([hash] or [chunkhash])
    // https://webpack.github.io/docs/list-of-plugins.html#occurrenceorderplugin
    new webpack.optimize.OccurrenceOrderPlugin(true),

    new webpack.NoErrorsPlugin(),
    // new ExtractTextPlugin('bundle.css', {
    //   allChunks: true
    // }),
  ]).concat(DEBUG ? [] : [
    // Search for equal or similar files and deduplicate them in the output
    // https://webpack.github.io/docs/list-of-plugins.html#dedupeplugin
    new webpack.optimize.DedupePlugin(),

    // Minimize all JavaScript output of chunks
    // https://github.com/mishoo/UglifyJS2#compressor-options
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        screw_ie8: true, // jscs:ignore requireCamelCaseOrUpperCaseIdentifiers
        warnings: false
      },
      comments: false
    })

    // // A plugin for a more aggressive chunk merging strategy
    // // https://webpack.github.io/docs/list-of-plugins.html#aggressivemergingplugin
    // new webpack.optimize.AggressiveMergingPlugin(),
  ]),

  // Choose a developer tool to enhance debugging
  // http://webpack.github.io/docs/configuration.html#devtool
  devtool: DEBUG ? 'eval' : false,
});
