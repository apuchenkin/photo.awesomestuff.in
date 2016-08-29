import React from 'react';

export default class Loader extends React.Component {
  componentDidMount() {
    console.log("startLoading");
  }

  componentWillUnmount() {
    console.log("stopLoading");
  }

  render() {
    return (
      <div className="loader" key="loader"><div className="accent"></div></div>
    );
  }
}
