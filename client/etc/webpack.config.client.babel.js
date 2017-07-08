import webpack from 'webpack';
import merge from 'webpack-merge';
import AssetsPlugin from 'assets-webpack-plugin';
import CompressionPlugin from 'compression-webpack-plugin';
import path from 'path';
import presetReact from 'babel-preset-react';
import presetEnv from 'babel-preset-env';

import base from './webpack.config.base.babel';

const isDevelopment = env => env === 'development';

const GLOBALS = DEBUG => ({
  'process.env.NODE_ENV': JSON.stringify(DEBUG ? 'development' : 'production'),
  'process.env.BROWSER': true,
  'process.env.ANALYTICS': JSON.stringify(process.env.ANALYTICS),
  'process.env.HOSTNAME': JSON.stringify(process.env.HOSTNAME),
  'process.env.STATIC_ENDPOINT': JSON.stringify(process.env.STATIC_ENDPOINT),
  'process.env.API_ENDPOINT': JSON.stringify(process.env.API_ENDPOINT),
  __DEV__: DEBUG,
  isBrowser: true,
});

module.exports = env => merge(base(env), {
  entry: './client.js',

  output: {
    path: path.resolve(__dirname, '../dist/assets'),
    publicPath: '/',
    filename: isDevelopment(env) ? '[name].js' : '[name].[hash].js',
  },

  target: 'web',
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, '../src'),
          path.resolve(__dirname, '../../common'),
        ],
        loader: 'babel-loader',
        options: {
          cacheDirectory: isDevelopment(env),
          babelrc: false,
          presets: [
            presetReact,
            presetEnv(isDevelopment
              ? null
              : {
                  "targets": {
                    "browsers": ["last 2 versions"]
                  },
                  useBuiltIns: true,
                  modules: false,
                }
            )
          ],
          plugins: [
            require('babel-plugin-transform-runtime'),
            require('babel-plugin-transform-object-rest-spread'),
            ...isDevelopment(env) ? [] : [
              require('babel-plugin-transform-react-constant-elements'),
            ],
          ],
        },
      },
    ],
  },

  plugins: ([
    // Define free variables
    // https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new webpack.DefinePlugin(GLOBALS(isDevelopment(env))),

    // Emit a file with assets paths
    // https://github.com/sporto/assets-webpack-plugin#options
    new AssetsPlugin({
      path: path.resolve(__dirname, '../dist'),
      filename: 'assets.json',
      // processOutput: x => `module.exports = ${JSON.stringify(x)};`,
    }),
  ]).concat(isDevelopment(env) ? [] : [
    new webpack.optimize.UglifyJsPlugin({
      beautify: false,
      mangle: {
        screw_ie8: true,
        keep_fnames: true
      },
      compress: {
        screw_ie8: true,
        warnings: false
      },
      comments: false
    }),
    new CompressionPlugin({
      asset: "[path].gz[query]",
      algorithm: "gzip",
      test: /\.(js|html)$/,
      threshold: 10240,
      minRatio: 0.8
    })
  ]),
  devtool: isDevelopment(env) ? 'eval' : false,
});
