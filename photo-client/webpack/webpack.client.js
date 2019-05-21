const baseConfig = require('./webpack.config');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const AssetsPlugin = require('assets-webpack-plugin');
const path = require('path');

module.exports = (env = {}) => Object.assign({}, {
  entry: './src/index.tsx',
  output: {
    path: path.join(__dirname, '../dist/client'),
    filename: env.production ? 'photo.[hash].js' : 'photo.bundle.js',
    chunkFilename: env.production ? '[name].[hash].js' : '[name].bundle.js',
    publicPath: '/',
  },
  optimization: {
    splitChunks: {
      // include all types of chunks
      chunks: 'all'
    }
  },
  plugins: [
    env.production && (new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: "[name].css",
      chunkFilename: "[id].css"
    })),
    new AssetsPlugin({
      path: path.join(__dirname, '../assets'),
      filename: 'assets.json',
    }),
  ].filter(Boolean),
}, baseConfig(env));