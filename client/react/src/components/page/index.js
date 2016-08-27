import React from 'react';
import Link from 'react-router/lib/Link';
import Loader from '../loader';

class Page extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      page: props.page || {},
      content: props.content || {}
    }
  }

  // componentDidMount() {
  //   // debugger;
  //   // this.props.route.connect(this);
  // }
  //
  // componentWillUnmount() {
  //   // debugger;
  // }

  componentWillReceiveProps(props) {
    let
      me = this,
      state = this.state;
    console.log("componentWillReceiveProps");

    if (props.route.path != this.props.route.path) {
      this.setState({content: props.content});
    }
  }

	render() {
		return (
      <div>
  			<div className="page" dangerouslySetInnerHTML={{__html: this.state.content}} ></div>
      </div>
		);
	}
}

Page.propTypes = {
  page: React.PropTypes.object.isRequired,
  content: React.PropTypes.string.isRequired
};

export default Page;
