var webpack = require( 'webpack' )

module.exports = {
  entry: {
    app: './src/index.js',
    packery: ['packery']
  },

  output: {
    path: './dist',
    filename: '[name].js'
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
      // LESS
      {
        test: /\.less$/,
        loader: 'style!css!less'
      },
    ],

    noParse: /\.elm$/
  },

  devServer: {
    inline: true,
    stats: 'errors-only',
    historyApiFallback: true,
    proxy: {
        '/api/v1*': {
            target: 'http://localhost:3000',
            rewrite: function(req) {
              req.url = req.url.replace(/^\/api\/v1/, '');
            },
            changeOrigin: true,
            secure: false,
        },
    },
  },

  plugins: [
    new webpack.optimize.CommonsChunkPlugin('common.js')
  ]
  // plugins: [
  //     // extract CSS into a separate file
  //     // new ExtractTextPlugin( './css/stylesheet.css', { allChunks: true } ),
  //
  //     // minify & mangle JS/CSS
  //     new webpack.optimize.UglifyJsPlugin({
  //         minimize:   true,
  //         compressor: { warnings: false },
  //         mangle:     true                      // TODO: need any exceptions?
  //     })
  //   ]
};
