import webpack from 'webpack';
import merge from 'webpack-merge';
import AssetsPlugin from 'assets-webpack-plugin';
import path from 'path';

import base from './webpack.config.base.babel';

const isDevelopment = env => env === 'development';

const GLOBALS = DEBUG => ({
  'process.env.NODE_ENV': JSON.stringify(DEBUG ? 'development' : 'production'),
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
        options: {
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
            'transform-runtime',
            'transform-object-rest-spread',
            ...isDevelopment(env) ? [] : [
              'transform-react-constant-elements',
            ],
          ],
        },
      },
    ],
  },

  plugins: [
    // Define free variables
    // https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new webpack.DefinePlugin(GLOBALS(isDevelopment(env))),
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
