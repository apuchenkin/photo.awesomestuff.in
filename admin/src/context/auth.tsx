import * as React from 'react';
import Auth from '@app/components/auth';

export type Login = (email: string, password: string) => void;
export type Logout = () => void;

interface Props {
  login: Login;
  logout: Logout;
  getToken: () => string;
}

const AUTH = 'AUTH';

// @ts-ignore
export const Context = React.createContext<Props>();

const AuthProvider: React.FunctionComponent = ({ children }) => {
  const [token, setToken] = React.useState<string>(localStorage.getItem(AUTH));

  return (
    <Context.Provider
      value={{
        getToken: () => token,
        login: async (email: string, password: string) => {
          const token = btoa(`${email}:${password}`)
          localStorage.setItem(AUTH, token);
          setToken(token);
        },
        logout: () => {
          localStorage.removeItem(AUTH);
          setToken(null);
        },
      }}
    >
      {token
        ? children
        : <Auth />
      }
    </Context.Provider>
  );
}

export default AuthProvider;