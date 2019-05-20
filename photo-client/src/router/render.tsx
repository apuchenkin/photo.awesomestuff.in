import * as React from 'react';
import createRender from 'found/lib/createRender';
// @ts-ignore
import ElementsRenderer from 'found/lib/ElementsRenderer';
// @ts-ignore
import StaticContainer from 'react-static-container';
import Loader from '@app/components/loader';
import { TransitionGroup } from 'react-transition-group';
import { RenderErrorArgs, RenderReadyArgs } from 'found';
import Fade from '@app/components/animation/fade';

const NotFound: React.FunctionComponent = () => <div>Not found</div>;
const ServiceUnavailable: React.FunctionComponent = () => <div>Service Unavailable</div>;

const renderError: React.FunctionComponent<RenderErrorArgs> = ({ error, ...props }) => (
  <TransitionGroup component={null}>
    <StaticContainer shouldUpdate>
    {
      error.status === 404
      ? <NotFound {...props} />
      : <ServiceUnavailable />
    }
    </StaticContainer>
  </TransitionGroup>
);

const renderPending = () => (
  <TransitionGroup component={null}>
    <StaticContainer />
    <Fade>
      <Loader />
    </Fade>
  </TransitionGroup>
);

const renderReady: React.FunctionComponent<RenderReadyArgs> = ({ elements, context, ...args }) => {
  context.data = args.routes.reduce((map, route, i) =>({
    ...map,
    [String(route.path)]: {
      // @ts-ignore
      params: args.routeParams[i],
      data: elements[i].props.data,
    }
  }), {});

  return (
    <TransitionGroup component={null}>
      <StaticContainer shouldUpdate>
        <ElementsRenderer elements={elements} />
      </StaticContainer>
    </TransitionGroup>
  );
};

export default createRender({
  renderError,
  renderPending,
  renderReady,
});
