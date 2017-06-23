import merge from 'webpack-merge';
import baseConfig from './webpack.config.server';
import { hostname } from '../src/etc/config.json';

const config = {
  output: {
    publicPath: `${hostname}${baseConfig.output.publicPath}`,
  },
};

export default merge(baseConfig, config);
