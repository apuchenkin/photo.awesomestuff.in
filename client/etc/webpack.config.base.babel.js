import webpack from 'webpack';
import postcssImport from 'postcss-import';
import autoprefixer from 'autoprefixer';
import cssNext from 'postcss-cssnext';
import cssNano from 'cssnano'
import path from 'path';

const isDevelopment = env => env === 'development';
// module.exports = ({ file, options, env }) => {
//   console.log(file, options, env);
//
//   return ({
//     parser: file.extname === '.sss' ? 'sugarss' : false,
//     plugins: {
//       'postcss-import': { root: file.dirname },
//       'postcss-cssnext': { browsers: 'last 2 version' },
//       autoprefixer: { browsers: 'last 2 version' },
//       cssnano: env === 'production' ? options.cssnano : false,
//     },
//   });
// };

const log = v => {
  console.log(v);
  return v;
}

module.exports = env => ({
  context: path.resolve(__dirname, '../src'),

  resolve: {
    extensions: ['*', '.js', '.css', '.html'],
    modules: ['src', 'node_modules'],
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
        ]
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
        ]
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
