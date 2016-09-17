import webpack from 'webpack';
// import ExtractTextPlugin from 'extract-text-webpack-plugin';
import autoprefixer from 'autoprefixer';
import path from 'path';

const DEBUG = !process.argv.includes('--release');
const VERBOSE = process.argv.includes('--verbose');

module.exports = {
  context: path.resolve(__dirname, '../src'),

  resolve: {
    root: path.resolve(__dirname, '../src'),
    extensions: ['', '.jsx', '.js', '.json', '.less'],
    modulesDirectories: ['node_modules'],
  },

  cache: DEBUG,
  debug: DEBUG,

  stats: {
    colors: true,
    reasons: DEBUG,
    hash: VERBOSE,
    version: VERBOSE,
    timings: true,
    chunks: VERBOSE,
    chunkModules: VERBOSE,
    cached: VERBOSE,
    cachedAssets: VERBOSE,
  },

  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, '../src'),
        ],
        loader: 'babel',
        query: {
          // https://github.com/babel/babel-loader#options
          cacheDirectory: DEBUG,

          // https://babeljs.io/docs/usage/options/
          babelrc: false,
          presets: [
            'react',
            'es2015',
            'stage-0',
          ],
          plugins: [
            'transform-runtime',
            'transform-decorators-legacy',
            // 'transform-class-properties',

            // 'transform-object-rest-spread',
            ...DEBUG ? [] : [
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
          'isomorphic-style',
          `css?${JSON.stringify({
            sourceMap: DEBUG,
            importLoaders: true,
            // CSS Modules https://github.com/css-modules/css-modules
            modules: true,
            localIdentName: DEBUG ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
            // CSS Nano http://cssnano.co/options/
            minimize: !DEBUG,
          })}`,
          'postcss',
        ]
      },
      {
        test: /\.less$/,
        loaders: [
          'isomorphic-style',
          `css?${JSON.stringify({
            sourceMap: DEBUG,
            minimize: !DEBUG,
            modules: true,
            localIdentName: DEBUG ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
            importLoaders: true
          })}`,
          'postcss',
          `less?${JSON.stringify({ sourceMap: DEBUG })}`,
        ]
      },
      {
        test: /\.json$/,
        loader: 'json',
      },
      {
        test: /\.(xml|html|txt|md)$/,
        loader: 'raw',
      },
      {
        test: /\.(png|jpg|jpeg|gif|svg|woff|woff2)(\?.*)?$/i,
        loader: 'url',
        query: {
          name: DEBUG ? '[path][name].[ext]' : '[hash].[ext]',
          limit: 10000,
        },
      },
      {
        test: /\.(eot|ttf|wav|mp3|ico)(\?.*)?$/,
        loader: 'file',
        query: {
          name: DEBUG ? '[path][name].[ext]' : '[hash].[ext]',
        },
      },
    ],
  },

  postcss: () => [
    autoprefixer({ browsers: 'last 2 versions' }),
  ],
};
