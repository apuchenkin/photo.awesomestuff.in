import webpack from 'webpack';
import ExtractTextPlugin from 'extract-text-webpack-plugin';
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
    // alias: {
    //   components: path.resolve(__dirname, 'src/components'),    // used for tests
    //   style: path.resolve(__dirname, 'src/style'),
    // },
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
            'es2015-minimal',
          ],
          plugins: [
            'transform-runtime',
            'transform-class-properties',
            'transform-decorators-legacy',
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
        loader: ExtractTextPlugin.extract('style', [
          `css?${JSON.stringify({
            sourceMap: DEBUG,
            importLoaders: true,
            // CSS Modules https://github.com/css-modules/css-modules
            // modules: true,
            localIdentName: DEBUG ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
            // CSS Nano http://cssnano.co/options/
            minimize: !DEBUG,
          })}`,
          'postcss',
        ].join('!')
        ),
      },
      {
        test: /\.less$/,
        loader: ExtractTextPlugin.extract('style', [
          `css?${JSON.stringify({ sourceMap: DEBUG, minimize: !DEBUG, importLoaders: 1 })}`,
          'postcss',
          `less?${JSON.stringify({ sourceMap: DEBUG })}`,
        ].join('!')
        ),
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
          name: DEBUG ? '[path][name].[ext]?[hash]' : '[hash].[ext]',
          limit: 10000,
        },
      },
      {
        test: /\.(eot|ttf|wav|mp3|ico)(\?.*)?$/,
        loader: 'file',
        query: {
          name: DEBUG ? '[path][name].[ext]?[hash]' : '[hash].[ext]',
        },
      },
    ],
  },

  postcss: () => [
    autoprefixer({ browsers: 'last 2 versions' }),
  ],
};
