import React from 'react';

export default class Loader extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      visible: true,
      class: null
    }
  }

  // componentWillEnter(cb) {
  //   console.log("componentWillEnter");
  //   this.setState({ class: 'loader-enter', visible: true }, cb);
  //   //  cb()
  // }
  //
  // componentWillLeave(cb) {
  //   console.log("componentWillLeave");
  //   this.setState({ class: 'loader-enter', visible: true }, () => setTimeout(cb, 200));
  // }
  //
  // componentDidEnter() {
  //   console.log("componentDidEnter");
  //   this.setState({ class: null, visible: true });
  // }
  //
  // componentDidLeave() {
  //   console.log("componentDidLeave");
  //   this.setState({ class: null, visible: false });
  // }

  render() {
    const className = ["loader", this.props.visible ? '' : 'hidden'].join(" ");

    return (
      <div className={className} key="loader"><div className="accent" /></div>
    );
  }
}
