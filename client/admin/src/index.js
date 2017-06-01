// import 'core-js/fn/object/assign';
import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/Main';

// Render the main component into the dom\
const div = document.createElement('div');
document.getElementsByTagName('body')[0].appendChild(div);
ReactDOM.render(<App />, div);
