import React from 'react';
import createRender from 'found/lib/createRender';
import ElementsRenderer from 'found/lib/ElementsRenderer';
import StaticContainer from 'react-static-container';
// import { CSSTransition, TransitionGroup } from 'react-transition-group';
import Loader from '../components/loader';
import NotFound from '../components/error/404';
import ServiceUnavailable from '../components/error/500';
import { actions } from '../store/cache';
// eslint-disable-next-line react/prop-types
const renderError = ({ error, ...props }) => (
  <div>
    <StaticContainer shouldUpdate>
      {error.status === 404 ? <NotFound {...props} /> : <ServiceUnavailable />}
    </StaticContainer>
    <Loader />
  </div>
);

// export default ({ Component, props }) => (
//   <TransitionGroup>
//     {
//       (Component && props) ? (
//         <Fade key="content" exit={false}><Component {...props} /></Fade>
//       ) : (
//
//       )
//     }
//   </TransitionGroup>
// );
// const Fade = ({ children, ...props }) => (
//   <CSSTransition
//     {...props}
//     timeout={150}
//     classNames="fade"
//   >
//     {children}
//   </CSSTransition>
// );

const renderPending = () => (
  <div>
    <StaticContainer />
    <Loader />
  </div>
);

// eslint-disable-next-line react/prop-types
const renderReady = ({ elements, routeIndices, context: { store } }) => {
  const cache = elements.map(e => e && e.props.data);

  store.dispatch(actions.cache(cache, routeIndices));

  return (
    <div>
      <StaticContainer shouldUpdate>
        <ElementsRenderer elements={elements} />
      </StaticContainer>
    </div>
  );
};

export default createRender({
  renderError,
  renderPending,
  renderReady,
});
