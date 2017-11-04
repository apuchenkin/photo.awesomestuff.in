import React from 'react';
import createRender from 'found/lib/createRender';
import ElementsRenderer from 'found/lib/ElementsRenderer';
import StaticContainer from 'react-static-container';
import { TransitionGroup } from 'react-transition-group';
import Fade from '../components/animation/fade';
import Loader from '../components/loader';
import NotFound from '../components/error/404';
import ServiceUnavailable from '../components/error/500';
import { actions } from '../store/cache';

// eslint-disable-next-line react/prop-types
const renderError = ({ error, ...props }) => (
  <TransitionGroup>
    <Fade key="content">
      <StaticContainer shouldUpdate>
        {error.status === 404 ? <NotFound {...props} /> : <ServiceUnavailable />}
      </StaticContainer>
    </Fade>
  </TransitionGroup>
);

const renderPending = () => (
  <TransitionGroup>
    <Fade key="content"><StaticContainer /></Fade>
    <Fade key="loader" enter={false}><Loader /></Fade>
  </TransitionGroup>
);

// eslint-disable-next-line react/prop-types
const renderReady = ({ elements, routeIndices, context: { store } }) => {
  const cache = elements.map(e => e && e.props.data);

  store.dispatch(actions.cache(cache, routeIndices));

  return (
    <TransitionGroup>
      <Fade key="content">
        <StaticContainer shouldUpdate>
          <ElementsRenderer elements={elements} />
        </StaticContainer>
      </Fade>
    </TransitionGroup>
  );
};

export default createRender({
  renderError,
  renderPending,
  renderReady,
});
