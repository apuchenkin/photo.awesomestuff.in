const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');
const config = require('./etc/config');

const GLOBALS = DEBUG => ({
  'process.env.NODE_ENV': JSON.stringify(DEBUG ? 'development' : 'production'),
  'process.env.BASENAME': JSON.stringify(process.env.BASENAME),
  'process.env.STATIC_ENDPOINT': JSON.stringify(process.env.STATIC_ENDPOINT),
  'process.env.API_ENDPOINT': JSON.stringify(process.env.API_ENDPOINT),
  __DEV__: DEBUG,
});

module.exports = env => ({
  context: path.resolve(__dirname, './src'),
  entry: './index.js',

  output: {
    path: path.resolve(__dirname, './dist'),
    publicPath: process.env.BASENAME || config.basename,
    filename: 'index.js',
  },
  resolve: {
    extensions: ['.jsx', '.js', '.json'],
    modules: [
      path.resolve(__dirname, 'node_modules'),
    ],
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, 'src'),
          path.resolve(__dirname, 'node_modules', 'common'),
        ],
        use: {
          loader: 'babel-loader',
          options: {
            babelrc: false,
            presets: [
              require('babel-preset-react'),
              require('babel-preset-env'),
            ],
            plugins: [
              require('babel-plugin-transform-object-rest-spread'),
            ],
          },
        },
      },
      {
        test: /\.json$/,
        use: 'json-loader',
      },
      {
        test: /\.less$/,
        exclude: /^(https?:)?\/\//,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            {
              loader: 'css-loader',
              options: {
                importLoaders: 1,
              },
            },
            'less-loader',
          ],
        }),
      },
    ],
  },
  plugins: [
    new webpack.LoaderOptionsPlugin({
      minimize: env === 'prod',
      debug: env !== 'prod',
    }),
    new ExtractTextPlugin('style.bundle.css'),
    new HtmlWebpackPlugin({
      template: 'index.ejs',
    }),
    new webpack.DefinePlugin(GLOBALS(env !== 'prod')),
  ],
  stats: {
    colors: true,
  },
  devtool: env === 'prod' ? 'source-map' : 'eval',
  devServer: {
    port: 9000,
    historyApiFallback: true,
    proxy: {
      '/api/v1': {
        logLevel: 'debug',
        target: 'http://localhost:3000',
        changeOrigin: true,
        pathRewrite: { '^/api/v1': '' },
      },
    },
  },
});
