var webpack = require( 'webpack' )

module.exports = {
  entry: './src/index.js',

  output: {
    path: './dist',
    filename: 'index.js'
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
            target: 'http://photo.awesomestuff.in',
            changeOrigin: true,
            secure: false,
        },
    },
  },

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
