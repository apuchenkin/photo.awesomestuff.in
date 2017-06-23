import webpack from 'webpack';
import merge from 'webpack-merge';
import AssetsPlugin from 'assets-webpack-plugin';
import path from 'path';

import base from './webpack.config.base.babel';

const GLOBALS = DEBUG => ({
  'process.env.NODE_ENV': DEBUG ? '"development"' : '"production"',
  'process.env.BROWSER': false,
  __DEV__: DEBUG,
  isBrowser: false,
});

module.exports = env => merge(base(env), {
  entry: ['babel-polyfill', './server.js'],
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

  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, '../src'),
          path.resolve(__dirname, '../lib'),
        ],
        loader: 'babel-loader',
        query: {
          // https://babeljs.io/docs/usage/options/
          babelrc: false,
          presets: [
            'react',
            ["env", {
              "targets": {
                "node": "current"
              },
              useBuiltIns: true,
            }]
          ],
          plugins: [
            // 'transform-runtime',
            // 'transform-decorators-legacy',
            // 'transform-class-properties',

            'transform-object-rest-spread',
            ...env ? [] : [
              // 'transform-react-remove-prop-types',
              // 'transform-react-constant-elements',
              // 'transform-react-inline-elements',
            ],
          ],
        },
      },
    ],
  },

  plugins: [

    // Define free variables
    // https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new webpack.DefinePlugin(GLOBALS(env === 'development')),

    // // Adds a banner to the top of each generated chunk
    // // https://webpack.github.io/docs/list-of-plugins.html#bannerplugin
    // new webpack.BannerPlugin('require("source-map-support").install();',
    //   { raw: true, entryOnly: false }),
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
