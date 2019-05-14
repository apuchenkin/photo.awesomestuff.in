import * as React from 'react';
import {
  BrowserRouter as Router,
} from 'react-router-dom';

import HTML5Backend from 'react-dnd-html5-backend';
import { DragDropContextProvider } from 'react-dnd';
import { ServiceProvider, CategoryProvider } from '@app/context';
import Main from '@app/components/main';

interface Props {
  config: any;
}

const App: React.FunctionComponent<Props> = ({ config }) => (
  <ServiceProvider config={{
    endpoint: config.apiEndpoint,
  }}>
    <CategoryProvider>
      <Router basename={config.basename} >
        <DragDropContextProvider backend={HTML5Backend}>
          <Main />
        </DragDropContextProvider>
      </Router>
    </CategoryProvider>
  </ServiceProvider>
);

export default App;
