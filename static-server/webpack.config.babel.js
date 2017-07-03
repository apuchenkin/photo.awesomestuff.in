import webpack from 'webpack';
import path from 'path';

const GLOBALS = DEBUG => ({
  'process.env.NODE_ENV': DEBUG ? '"development"' : '"production"',
  __DEV__: DEBUG,
});

const isDev = env => env !== 'production';

module.exports = env => ({
  entry: ['babel-polyfill', './index.js'],
  target: 'node',

  context: path.resolve(__dirname, 'src'),

  resolve: {
    extensions: ['.jsx', '.js', '.json'],
    modules: ['node_modules'],
  },

  externals: [
    /^[a-z\-0-9]+$/,
  ],

  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: [
          path.resolve(__dirname, 'etc'),
          path.resolve(__dirname, 'src'),
          path.resolve(__dirname, '..', 'common'),
        ],
        loader: 'babel-loader',
      },
      {
        test: /\.json$/,
        loader: 'json-loader',
      },
      // {
      //   test: /\.(png|jpg|jpeg|gif|ico|svg|woff|woff2)(\?.*)?$/i,
      //   loader: 'url',
      //   query: {
      //     name: DEBUG ? '[path][name].[ext]' : '[hash].[ext]',
      //     limit: 10000,
      //   },
      // },
      // {
      //   test: /\.(eot|ttf|wav|mp3)(\?.*)?$/,
      //   loader: 'file',
      //   query: {
      //     name: DEBUG ? '[path][name].[ext]' : '[hash].[ext]',
      //   },
      // },
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
    new webpack.DefinePlugin({
      ...GLOBALS(isDev(env)),
    }),
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
});
