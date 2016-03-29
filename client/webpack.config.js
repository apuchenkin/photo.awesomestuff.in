var webpack = require( 'webpack' )
var autoprefixer = require('autoprefixer');
var precss = require('precss');
var ExtractTextPlugin = require("extract-text-webpack-plugin");

module.exports = {
  entry: {
    app: './src/index.js',
    vendor: ['packery', 'perfect-scrollbar']
  },

  output: {
    path: './dist',
    filename: '[name].js',
    chunkFilename: "[id].js"
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm']
  },

  module: {
    loaders: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack'
      },
      {
        test: /\.css$/,
        loader: ExtractTextPlugin.extract("style-loader", "css-loader!postcss-loader")
      },
      {
        test: /\.less$/,
        loader: ExtractTextPlugin.extract("style-loader", "css-loader!postcss-loader!less-loader")
      }, {
        test: /\.(eot|woff|woff2|ttf|svg|png|jpg)/,
        loader: 'url-loader?limit=30000'
      }
    ],

    noParse: /\.elm$/
  },

  postcss: function () {
    return [autoprefixer, precss];
  },

  devServer: {
    inline: true,
    stats: 'errors-only',
    historyApiFallback: true,
    proxy: {
        '/api/v1*': {
            target: 'http://192.168.138.34:3000',
            rewrite: function(req) {
              req.url = req.url.replace(/^\/api\/v1/, '');
            },
            changeOrigin: true,
            secure: false,
        },
    },
  },

  plugins: [
    new ExtractTextPlugin("[name].css"),
    new webpack.optimize.CommonsChunkPlugin('common.js')
  ]
};
