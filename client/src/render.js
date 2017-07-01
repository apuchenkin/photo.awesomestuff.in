import React from 'react';
import createRender from 'found/lib/createRender';
import ElementsRenderer from 'found/lib/ElementsRenderer';
import Loader from './components/loader/loader';
import { buildMeta, metaUpdate } from './lib/meta';

// eslint-disable-next-line react/prop-types
const renderError = ({ error }) => (
  <div>
    {error.status === 404 ? 'Not found' : 'Error'}
  </div>
);

export const serverRender = createRender({
  renderError,
});

export const clientRender = createRender({
  renderError,
  renderPending: () => <Loader />,
  // eslint-disable-next-line react/prop-types
  renderReady: ({ location, elements, context: { store, intl } }) => {
    const meta = buildMeta(location, store, intl);
    metaUpdate(meta, location);

    return <ElementsRenderer elements={elements} />;
  },
});
