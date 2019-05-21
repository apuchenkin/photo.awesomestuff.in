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
import { NotFound, ServiceUnavailable } from '@app/components/error';
import Main from '@app/page/main';
import { Header } from '@app/components/home';
import { FoundProvider } from '@app/context';

const renderError: React.FunctionComponent<RenderErrorArgs> = ({ error, ...props }) => (
  <TransitionGroup component={null}>
    <StaticContainer shouldUpdate>
      <FoundProvider match={props}>
        <Main header={<Header />}>
          {
            error.status === 404
            ? <NotFound {...props} />
            : <ServiceUnavailable />
          }
        </Main>
      </FoundProvider>
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
