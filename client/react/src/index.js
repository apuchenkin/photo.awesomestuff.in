import React from 'react';
import ReactDOM from 'react-dom';
import Router from 'react-router/lib/Router';
import match from 'react-router/lib/match';
import history from 'react-router/lib/browserHistory';

// import ExtraDataProvider from './lib/provider.js';
import './assets/fontello/css/fontello.css';
import './style/main.less';
import routes         from './routes';

function createElement(Component, props) {
  return <Component {...props} {...props.route.props} />
}

match({ history, routes }, (error, redirectLocation, renderProps) => {
  ReactDOM.render(
    // <ExtraDataProvider initialState={initialState}>
      <Router {...renderProps} createElement={createElement} />
    // </ExtraDataProvider>
    , document.getElementById('react-view')
  );
})
