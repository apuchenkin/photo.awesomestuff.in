import * as React from 'react';
import factory, { Services, Config } from '@app/service/factory';

// @ts-ignore
export const Context = React.createContext<Services>();

interface Props {
  config: Config;
}

const ServiceProvider: React.FunctionComponent<Props> = ({ config, children }) => (
  <Context.Provider
    value={factory(config)}
  >
    {children}
  </Context.Provider>
);

export default ServiceProvider;
