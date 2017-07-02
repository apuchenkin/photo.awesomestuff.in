import webpack from 'webpack';
import merge from 'webpack-merge';
import AssetsPlugin from 'assets-webpack-plugin';
import CompressionPlugin from 'compression-webpack-plugin';
import path from 'path';

import base from './webpack.config.base.babel';

const isDevelopment = env => env === 'development';

const GLOBALS = DEBUG => ({
  'process.env.NODE_ENV': JSON.stringify(DEBUG ? 'development' : 'production'),
  'process.env.BROWSER': true,
  __DEV__: DEBUG,
  isBrowser: true,
});

module.exports = env => merge(base(env), {
  entry: ['babel-polyfill', './client.js'],

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
          path.resolve(__dirname, '../lib'),
        ],
        loader: 'babel-loader',
        options: {
          cacheDirectory: isDevelopment(env),
          babelrc: false,
          presets: [
            'react',
            isDevelopment
              ? 'env'
              : ["env", {
                  "targets": {
                    "browsers": ["last 2 versions"]
                  },
                  useBuiltIns: true,
                  modules: false,
                }],
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
