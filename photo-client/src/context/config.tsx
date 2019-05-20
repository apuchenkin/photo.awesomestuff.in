import * as React from 'react';

// @ts-ignore
export const Context = React.createContext<Config>();

interface Props {
  config: Config;
}

const ConfigProvider: React.FunctionComponent<Props> = ({ config, children }) => (
  <Context.Provider
    value={config}
  >
    {children}
  </Context.Provider>
);

export default ConfigProvider;
