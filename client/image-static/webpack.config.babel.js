import webpack from 'webpack';
import path from 'path';

const DEBUG = !process.argv.includes('--release');
const VERBOSE = process.argv.includes('--verbose');

const GLOBALS = {
  'process.env.NODE_ENV': DEBUG ? '"development"' : '"production"',
  'process.env.BROWSER': false,
  __DEV__: DEBUG,
};

module.exports = {
  target: 'node',
  entry: './index.js',

  context: path.resolve(__dirname, './src'),

  resolve: {
    root: path.resolve(__dirname, './src'),
    extensions: ['', '.jsx', '.js', '.json'],
    modulesDirectories: ['node_modules'],
  },

  externals: [
    /^[a-z\-0-9]+$/,
  ],

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
          path.resolve(__dirname, 'src'),
          path.resolve(__dirname, '..', 'lib'),
        ],
        loader: 'babel',
        query: {
          cacheDirectory: true,
          babelrc: false,
          presets: ['babel-preset-node6', 'babel-preset-es2015-minimal'].map(require.resolve),
          plugins: ['babel-plugin-transform-runtime'].map(require.resolve)
        }
      },
      {
        test: /\.json$/,
        loader: require.resolve('json-loader'),
      },
      {
        test: /\.(png|jpg|jpeg|gif|ico|svg|woff|woff2)(\?.*)?$/i,
        loader: 'url',
        query: {
          name: DEBUG ? '[path][name].[ext]' : '[hash].[ext]',
          limit: 10000,
        },
      },
      {
        test: /\.(eot|ttf|wav|mp3)(\?.*)?$/,
        loader: 'file',
        query: {
          name: DEBUG ? '[path][name].[ext]' : '[hash].[ext]',
        },
      },
    ],
  },

  output: {
    path: path.resolve(__dirname, './dist'),
    publicPath: '/',
    filename: 'index.js',
    chunkFilename: 'index.[name].js',
    libraryTarget: 'commonjs2',
  },

  plugins: [
    // new webpack.DefinePlugin({ ...GLOBALS }),
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
};
