import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import store from './store/configure';
import App from './components/Main';

const div = document.createElement('div');
document.getElementsByTagName('body')[0].appendChild(div);
ReactDOM.render((
  <Provider store={store}>
    <App />
  </Provider>
  ), div);
