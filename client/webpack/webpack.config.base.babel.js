import webpack from 'webpack';
import path from 'path';

const isDevelopment = env => env === 'development';

module.exports = env => ({
  context: path.resolve(__dirname, '../src'),

  resolve: {
    extensions: ['*', '.js', '.css', '.html'],
    modules: [
      'node_modules',
      path.resolve(__dirname, '..', 'node_modules'),
    ],
  },

  module: {
    rules: [
      {
        test: /\.css$/,
        loaders: [
          'style-loader',
          `css-loader?${JSON.stringify({
            sourceMap: isDevelopment(env),
            importLoaders: isDevelopment(env) ? 0 : 1,
            modules: false,
            localIdentName: isDevelopment(env) ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
            minimize: !isDevelopment(env),
          })}`,
          isDevelopment(env) ? null : 'postcss-loader'
        ].filter(Boolean)
      },
      {
        test: /\.less$/,
        include: [
          path.resolve(__dirname, '../src'),
        ],
        loaders: [
          'isomorphic-style-loader',
          {
            loader: 'css-loader',
            options: {
              minimize: !isDevelopment(env),
              sourceMap: isDevelopment(env),
              modules: true,
              localIdentName: isDevelopment(env) ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
              importLoaders: isDevelopment(env) ? 1 : 2
            }
          },
          isDevelopment(env) ? null : 'postcss-loader',
          { loader: 'less-loader', options: { sourceMap: isDevelopment(env) } },
        ].filter(Boolean)
      },
      {
        test: /\.json$/,
        loader: 'json-loader',
      },
      {
        test: /\.(xml|html|txt|md)$/,
        loader: 'raw-loader',
      },
      {
        test: /\.(png|jpg|jpeg|gif|ico|svg|woff|woff2)(\?.*)?$/i,
        loader: 'url-loader',
        query: {
          name: isDevelopment(env) ? '[path][name].[ext]' : '[hash].[ext]',
          limit: 10000,
        },
      },
      {
        test: /\.(eot|ttf|wav|mp3)(\?.*)?$/,
        loader: 'file-loader',
        query: {
          name: isDevelopment(env) ? '[path][name].[ext]' : '[hash].[ext]',
        },
      },
    ],
  },
  plugins: (isDevelopment(env) ? [] : [
    new webpack.LoaderOptionsPlugin({
      minimize: true,
      debug: false,
    }),
  ]),
});
