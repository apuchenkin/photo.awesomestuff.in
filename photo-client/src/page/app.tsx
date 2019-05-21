import * as React from 'react';
import { FoundProvider } from '@app/context';
import { Match } from 'found';

const App: React.FunctionComponent<Match> = ({ children, ...match }) => (
  <FoundProvider match={match}>
    {children}
  </FoundProvider>
);

export default App;
