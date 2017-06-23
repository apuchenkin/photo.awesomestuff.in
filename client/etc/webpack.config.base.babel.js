import webpack from 'webpack';
import autoprefixer from 'autoprefixer';
import path from 'path';

const isDevelopment = env => env === 'development';

module.exports = env => ({
  context: path.resolve(__dirname, '../src'),

  // resolve: {
  //   root: path.resolve(__dirname, '../src'),
  //   extensions: ['', '.jsx', '.js', '.json', '.less'],
  //   modulesDirectories: ['node_modules'],
  // },
  resolve: {
    extensions: ['*', '.js', '.css', '.html'],
    modules: ['src', 'node_modules'],
    // alias: {
    //   app: path.resolve(rootFolder, 'src/app'),
    // },
  },

  // cache: DEBUG,
  // debug: DEBUG,
  //
  // stats: {
  //   colors: true,
  //   reasons: DEBUG,
  //   hash: VERBOSE,
  //   version: VERBOSE,
  //   timings: true,
  //   chunks: VERBOSE,
  //   chunkModules: VERBOSE,
  //   cached: VERBOSE,
  //   cachedAssets: VERBOSE,
  // },

  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, '../src'),
          path.resolve(__dirname, '../lib'),
        ],
        loader: 'babel-loader',
        query: {
          // https://github.com/babel/babel-loader#options
          // cacheDirectory: isDevelopment(env),

          // https://babeljs.io/docs/usage/options/
          babelrc: false,
          presets: [
            'react',
            'env',
          ],
          plugins: [
            // 'transform-runtime',
            // 'transform-decorators-legacy',
            // 'transform-class-properties',

            'transform-object-rest-spread',
            ...isDevelopment(env) ? [] : [
              'transform-react-remove-prop-types',
              'transform-react-constant-elements',
              'transform-react-inline-elements',
            ],
          ],
        },
      },
      {
        test: /\.css$/,
        loaders: [
          'style-loader',
          `css-loader?${JSON.stringify({
            sourceMap: isDevelopment(env),
            importLoaders: 1,
            modules: true,
            localIdentName: isDevelopment(env) ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
            minimize: !isDevelopment(env),
          })}`,
          'postcss-loader',
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
              sourceMap: isDevelopment(env),
              minimize: !isDevelopment(env),
              modules: true,
              localIdentName: isDevelopment(env) ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
              importLoaders: 2
            }
          },
          'postcss-loader',
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

  // postcss: () => [
  //   autoprefixer({ browsers: 'last 2 versions' }),
  // ],
});
