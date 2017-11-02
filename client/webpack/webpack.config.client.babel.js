import webpack from 'webpack';
import merge from 'webpack-merge';
import AssetsPlugin from 'assets-webpack-plugin';
import CompressionPlugin from 'compression-webpack-plugin';
import path from 'path';
import presetReact from 'babel-preset-react';
import presetEnv from 'babel-preset-env';
import base from './webpack.config.base.babel';
import { BundleAnalyzerPlugin } from 'webpack-bundle-analyzer';

const isDevelopment = env => env === 'development';

export default env => merge(base(env), {
  target: 'web',

  entry: [
    'babel-polyfill',
    path.resolve(__dirname, '..', 'src', 'client.js'),
  ],

  output: {
    path: path.resolve(__dirname, '..', 'dist', 'assets'),
    publicPath: '/',
    filename: isDevelopment(env) ? '[name].js' : '[name].[hash].js',
  },

  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, '..', 'src'),
          path.resolve(__dirname, '..', 'node_modules', 'common'),
        ],
        loader: 'babel-loader',
        options: {
          cacheDirectory: isDevelopment(env),
          babelrc: false,
          presets: [
            presetReact,
            presetEnv({
              useBuiltIns: true,
              modules: false,
              "targets": {
                "browsers": ["last 2 versions"]
              },
            })
          ],
          plugins: [
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
    new webpack.DefinePlugin({
      __DEV__: isDevelopment(env),
      isBrowser: true,
    }),
    new webpack.EnvironmentPlugin({
      BROWSER: true,
      NODE_ENV: JSON.stringify(isDevelopment(env) ? 'development' : 'production'),
      ANALYTICS: JSON.stringify(process.env.ANALYTICS),
      HOSTNAME: JSON.stringify(process.env.HOSTNAME),
      STATIC_ENDPOINT: JSON.stringify(process.env.STATIC_ENDPOINT),
      API_ENDPOINT: JSON.stringify(process.env.API_ENDPOINT),
    }),
    new AssetsPlugin({
      path: path.resolve(__dirname, '../dist'),
      filename: 'assets.json',
    }),
  ]).concat(isDevelopment(env) ? [] : [
    new webpack.optimize.UglifyJsPlugin({
      beautify: false,
      sourceMap: !isDevelopment(env),
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
    }),
    new BundleAnalyzerPlugin(),
  ]),
  devtool: isDevelopment(env) ? 'eval-source-map' : 'source-map',
});
