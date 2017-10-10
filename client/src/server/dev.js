import webpack from 'webpack';
import webpackMiddleware from 'webpack-dev-middleware';
import config from '../../webpack/webpack.config.client.babel';

const DEV = 'development';

export default webpackMiddleware(webpack(config(DEV)), {
  noInfo: false,
  quiet: false,
  lazy: true,
  watchOptions: {
    aggregateTimeout: 300,
    poll: true,
  },
  publicPath: '/assets/',
  stats: {
    colors: true,
  },
  serverSideRender: false,
});
