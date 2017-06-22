import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { BrowserRouter as Router } from 'react-router-dom';
import store from './store/configure';
import App from './components/Main';

const div = document.createElement('div');
document.getElementsByTagName('body')[0].appendChild(div);
ReactDOM.render((
  <Provider store={store}>
    <Router>
      <App />
    </Router>
  </Provider>
  ), div);