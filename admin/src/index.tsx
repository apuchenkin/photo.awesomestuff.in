import * as React from "react";
import * as ReactDOM from "react-dom";
import App from './app';
// @ts-ignore
import * as config from '../etc/config';

ReactDOM.render(
  <App config={config} />,
  document.getElementById("react-root")
);