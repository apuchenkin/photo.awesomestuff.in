import webpack from 'webpack';
import merge from 'webpack-merge';
import AssetsPlugin from 'assets-webpack-plugin';
import path from 'path';
import presetReact from 'babel-preset-react';
import presetEnv from 'babel-preset-env';
import base from './webpack.config.base.babel';
import nodeExternals from 'webpack-node-externals';

const isDevelopment = env => env === 'development';

export default env => merge(base(env), {
  target: 'node',

  entry: [
    'babel-polyfill',
    path.resolve(__dirname, '..', 'src', 'server', 'app.js'),
  ],

  output: {
    path: path.resolve(__dirname, '..', 'dist'),
    publicPath: '/assets/',
    filename: 'server.js',
    chunkFilename: 'server.[name].js',
    libraryTarget: 'commonjs2',
  },

  externals: [
    /^\.\/assets$/,
    nodeExternals({
      whitelist: [
        /^common.*/,
      ],
    }),
  ],

  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, '..', 'src'),
          path.resolve(__dirname, '..', 'node_modules', 'common', 'service'),
        ],
        loader: 'babel-loader',
        options: {
          // https://babeljs.io/docs/usage/options/
          babelrc: false,
          presets: [
            presetReact,
            presetEnv({
              "targets": {
                "node": "current"
              },
              useBuiltIns: true,
              modules: false,
            }),
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

  plugins: [
    new webpack.DefinePlugin({
      __DEV__: isDevelopment(env),
      isBrowser: false,
    }),
    new webpack.EnvironmentPlugin({
      NODE_ENV: JSON.stringify(isDevelopment(env) ? 'development' : 'production'),
      BROWSER: false,
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
