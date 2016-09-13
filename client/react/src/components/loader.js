import React from 'react';

export default class Loader extends React.Component {

  shouldComponentUpdate() {
    return false;
  }

  render() {
    return (
      <div className="loader" key="loader"><div className="accent" /></div>
    );
  }
}
