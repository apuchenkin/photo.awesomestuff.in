const path = require('path');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.js',
  output: {
    filename: 'main.bundle.js',
    publicPath: '/',
  },
  resolve: {
    // modules: [path.resolve(__dirname, '..', '..', 'src'), 'node_modules'],
    extensions: ['.js', '.jsx'],
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader',
      },
      {
        test: /\.json$/,
        use: 'json-loader',
      },
      {
        test: /\.less$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: ['css-loader', 'less-loader'],
        }),
      },
    ],
  },
  plugins: [
    new ExtractTextPlugin('style.bundle.css'),
    new HtmlWebpackPlugin(),
  ],
  stats: {
    colors: true,
  },
  devtool: 'inline-source-map',
  devServer: {
    port: 9000,
    proxy: {
      '/api/v1': {
        target: 'http://photo.awesomestuff.in:3000',
        pathRewrite: { '^/api/v1': '' },
      },
    },
  },
};
