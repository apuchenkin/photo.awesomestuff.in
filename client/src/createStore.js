import { createStore, applyMiddleware, compose } from 'redux';
import reducers from './reducers';

export default function configureStore(initialState) {
  let store;

  if (__DEV__) {
    // eslint-disable-next-line
    const createLogger = require('redux-logger');

    const logger = createLogger();

    // https://github.com/zalmoxisus/redux-devtools-extension#redux-devtools-extension
    let devToolsExtension = f => f;
    if (process.env.BROWSER && window.devToolsExtension) {
      devToolsExtension = window.devToolsExtension();
    }

    const enhancer = compose(
      applyMiddleware(logger),
      devToolsExtension,
    );

    store = createStore(reducers, initialState, enhancer);
  } else {
    store = createStore(reducers, initialState);
  }

  return store;
}
