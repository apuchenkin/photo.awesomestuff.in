import webpack from 'webpack';
import ExtractTextPlugin from 'extract-text-webpack-plugin';
// import HtmlWebpackPlugin from 'html-webpack-plugin';
import AssetsPlugin from 'assets-webpack-plugin';
import autoprefixer from 'autoprefixer';
import path from 'path';
import config from './src/config/config.json';

const ENV = process.env.NODE_ENV || 'development';
const DEBUG = ENV !== 'production';
console.log(DEBUG);

module.exports = {
  context: path.resolve(__dirname, "src"),
  entry: './index.js',

  output: {
    path: path.resolve(__dirname, "build"),
    publicPath: '/',
    filename: 'bundle.js'
  },

  resolve: {
    extensions: ['', '.jsx', '.js', '.json', '.less'],
    modulesDirectories: [
      path.resolve(__dirname, "src/lib"),
      path.resolve(__dirname, "node_modules"),
      'node_modules'
    ],
    alias: {
      components: path.resolve(__dirname, "src/components"),    // used for tests
      style: path.resolve(__dirname, "src/style")
    }
  },

  module: {
    preLoaders: [
      {
        test: /\.jsx?$/,
        exclude: /src\//,
        loader: 'source-map'
      }
    ],
    loaders: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        loader: 'babel',
        query: {
          // https://github.com/babel/babel-loader#options
          cacheDirectory: DEBUG,

          // https://babeljs.io/docs/usage/options/
          babelrc: false,
          presets: [
            'react',
            'es2015-minimal'
          ],
          plugins: [
            'transform-runtime',
            "transform-class-properties",
            "transform-decorators-legacy",
            "transform-object-rest-spread",
            ...DEBUG ? [] : [
              'transform-react-remove-prop-types',
              'transform-react-constant-elements',
              'transform-react-inline-elements'
            ]
          ]
        }
      },
      {
        test: /\.css$/,
        loader: ExtractTextPlugin.extract('style-loader', [
          `css-loader?${JSON.stringify({
            sourceMap: DEBUG,
            importLoaders: 1,
            // CSS Modules https://github.com/css-modules/css-modules
            // modules: true,
            localIdentName: DEBUG ? '[name]_[local]_[hash:base64:3]' : '[hash:base64:4]',
            // CSS Nano http://cssnano.co/options/
            minimize: !DEBUG
          })}`,
          'postcss-loader'
        ].join('!')
        )
      },
      {
        test: /\.less$/,
        loader: ExtractTextPlugin.extract('style-loader', [
          `css-loader?${JSON.stringify({ sourceMap: DEBUG, minimize: !DEBUG, importLoaders: 1})}`,
          'postcss-loader',
          `less-loader?${JSON.stringify({ sourceMap: DEBUG})}`
        ].join('!')
        )
      },
      {
        test: /\.json$/,
        loader: 'json'
      },
      {
        test: /\.(xml|html|txt|md)$/,
        loader: 'raw'
      },
      {
        test: /\.(png|jpg|jpeg|gif|svg|woff|woff2)(\?.*)?$/i,
        loader: 'url',
        query: {
          name: DEBUG ? '[path][name].[ext]?[hash]' : '[hash].[ext]',
          limit: 10000
        }
      },
      {
        test: /\.(eot|ttf|wav|mp3)(\?.*)?$/,
        loader: 'file',
        query: {
          name: DEBUG ? '[path][name].[ext]?[hash]' : '[hash].[ext]'
        }
      }
    ]
  },

  postcss: () => [
    autoprefixer({ browsers: 'last 2 versions' })
  ],

  plugins: ([
    new webpack.NoErrorsPlugin(),
    new ExtractTextPlugin('bundle.css', {
      allChunks: true
    }),
    new webpack.optimize.DedupePlugin(),
    new webpack.DefinePlugin({
      'process.env': JSON.stringify({ NODE_ENV: ENV })
    }),
    // Emit a file with assets paths
    // https://github.com/sporto/assets-webpack-plugin#options
    new AssetsPlugin({
      path: path.resolve(__dirname, './build')
      // filename: 'assets.js',
      // processOutput: x => `module.exports = ${JSON.stringify(x)};`
    })
  ]).concat(ENV==='production' ? [
    new webpack.optimize.OccurrenceOrderPlugin(),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        warnings: false
      },
      comments: false
    })
  ] : []),

  stats: { colors: true },

  node: {
    global: true,
    process: false,
    Buffer: false,
    __filename: false,
    __dirname: false,
    setImmediate: false
  },

  devtool: ENV==='production' ? 'source-map' : 'eval',

  devServer: {
    port: process.env.PORT || 8080,
    host: '0.0.0.0',
    colors: true,
    publicPath: '/',
    contentBase: './src',
    historyApiFallback: true,
    proxy: {
      '/api/v1*': {
        target: config.apiProxy,
        rewrite(req) {
          req.url = req.url.replace(/^\/api\/v1/, '');
        },
        changeOrigin: true,
        secure: false
      }
    }
  }
};
