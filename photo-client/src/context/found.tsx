import * as React from 'react';
import { Match } from '@app/router';

// @ts-ignore
export const Context = React.createContext<Match>();

interface Props {
  match: Match;
}

const FoundProvider: React.FunctionComponent<Props> = ({ match, children }) => (
  <Context.Provider
    value={match}
  >
    {children}
  </Context.Provider>
);

export default FoundProvider;
