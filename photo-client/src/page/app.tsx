import * as React from 'react';
import { ConfigProvider, FoundProvider, PageProvider } from '@app/context';
import { Match } from 'found';

interface Props extends Match {
  pages: Page[],
  config: Config;
}

const App: React.FunctionComponent<Props> = ({ children, pages, config, ...match }) => (
  <PageProvider pages={pages}>
    <FoundProvider match={match}>
      <ConfigProvider config={config}>
        {children}
      </ConfigProvider>
    </FoundProvider>
  </PageProvider>
);

export default App;
