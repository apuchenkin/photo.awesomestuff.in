import React from 'react';
import createRender from 'found/lib/createRender';
import ElementsRenderer from 'found/lib/ElementsRenderer';
import StaticContainer from 'react-static-container';
import Loader from './components/loader/loader';
import { buildMeta, metaUpdate } from './lib/meta';
import NotFound from './components/error/404';
import ServiceUnavailable from './components/error/500';

// eslint-disable-next-line react/prop-types
const renderError = ({ error, ...props }) => (
  <div>
    <StaticContainer shouldUpdate>
      {error.status === 404 ? <NotFound {...props} /> : <ServiceUnavailable />}
    </StaticContainer>
    <Loader />
  </div>
);

const renderPending = () => (
  <div>
    <StaticContainer>
      {null}
    </StaticContainer>
    <Loader />
  </div>
);

// eslint-disable-next-line react/prop-types
const renderReady = ({ elements }) => (
  <div>
    <StaticContainer shouldUpdate>
      <ElementsRenderer elements={elements} />
    </StaticContainer>
  </div>
);

export const serverRender = createRender({
  renderError,
  renderPending,
  renderReady,
});

export const clientRender = createRender({
  renderError,
  renderPending,
  // eslint-disable-next-line react/prop-types
  renderReady: ({ location, elements, context: { store, intl } }) => {
    const meta = buildMeta(location, store, intl);
    metaUpdate(meta, location);

    return renderReady({ elements });
  },
});
