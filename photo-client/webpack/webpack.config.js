const path = require('path');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
// const TsConfigPathsPlugin = require('awesome-typescript-loader').TsConfigPathsPlugin;

module.exports = (env = {}) => ({
  mode: env.production ? 'production' : 'development',
  resolve: {
    extensions: ['.js', '.jsx', '.ts', '.tsx', '.css', '.scss', '.json'],
    // plugins: [
    //   new TsConfigPathsPlugin(/* { tsconfig, compiler } */)
    // ],
    alias: {
      '@app': path.resolve(__dirname, '../src'),
      '@assets': path.resolve(__dirname, '../assets'),
    },
  },
  // resolveLoader: {
  //   modules: [path.resolve(__dirname, "../node_modules")],
  // },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        exclude: /node_modules/,
        loader: 'awesome-typescript-loader',
      },
      { enforce: 'pre', test: /\.js$/, loader: 'source-map-loader' },
      {
        test: /\.css$/,
        use: [
          env.production ? MiniCssExtractPlugin.loader : 'style-loader',
          {
            loader: 'css-loader',
            options: { importLoaders: 1 },
          },
          'postcss-loader',
        ]
      },
      {
        test: /\.scss$/,
        use: [
          'isomorphic-style-loader',
          {
            loader: 'css-loader',
            options: {
              modules: true,
              importLoaders: 2
            },
          },
          'postcss-loader',
          'sass-loader',
        ],
      },
      {
        test: /\.(png|jpg|gif|ico)$/,
        use : [
          {
            loader: 'url-loader',
            options: {
              limite: 8192
            }
          }
        ]
      }
    ],
  },
  devtool: 'source-map',
  devServer: {
    historyApiFallback: true,
  },
});
