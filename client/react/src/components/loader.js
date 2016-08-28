import React from 'react';

export default class Loader extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      transition: false
    }
  }

  componentDidMount() {
    // this.setState({transition: true});
    // setTimeout(() => this.setState({transition: false}), 100);

    console.log("startLoading");
  }

  componentWillUnmount() {
    console.log("stopLoading");
    // this.setState({transition: true});
    // setTimeout(() => this.setState({transition: false}), 100);
  }

  render() {
    return (
      <div className="loader" key="loader"><div className="accent"></div></div>
    );
  }
}
