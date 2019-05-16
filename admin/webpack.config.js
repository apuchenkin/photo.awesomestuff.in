const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: "./src/index.tsx",
  output: {
      filename: "bundle.js",
      publicPath: '/',
      path: __dirname + "/dist"
  },

  devtool: "source-map",

  resolve: {
      extensions: [".ts", ".tsx", ".js", ".json", ".scss"],
      alias: {
        '@app': path.resolve(__dirname, './src'),
      },
  },

  module: {
      rules: [
          { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
          { enforce: "pre", test: /\.js$/, loader: "source-map-loader" },
          {
            test: /\.scss$/,
            use: [
              'style-loader',
              'css-loader',
              'sass-loader',
            ],
          },
      ]
  },

  externals: {
      "react": "React",
      "react-dom": "ReactDOM"
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: './src/index.ejs',
    }),
  ],

  devServer: {
    port: 9000,
    historyApiFallback: true,
    proxy: {
      '/api/v1': {
        logLevel: 'debug',
        target: 'http://localhost:3000',
        changeOrigin: true,
        pathRewrite: { '^/api/v1': '/admin' },
      },
    },
  },
};