import * as React from 'react';
import factory, { Services } from '@app/service/factory';
import { Context as AuthContext } from './auth';

// @ts-ignore
export const Context = React.createContext<Services>();

interface Props {
  endpoint: string;
}

const ServiceProvider: React.FunctionComponent<Props> = ({ endpoint, children }) => {
  const { getToken } = React.useContext(AuthContext);

  return (
    <Context.Provider
      value={factory({
        endpoint,
        token: getToken(),
      })}
    >
      {children}
    </Context.Provider>
  );
}

export default ServiceProvider;
