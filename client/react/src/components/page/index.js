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

  componentDidMount() {
    // debugger;
  }

  componentWillReceiveProps(props) {
    let
      me = this,
      state = this.state;

    if (props.page.id != state.page.id) {
      this.setState({
        isLoading: true
      });
      props.route.parent.resolve(props.params).then(data => {
        this.setState(Object.assign(data, {
          content: data.page.content,
          isLoading: false
        }))
      })
    }
  }

	render() {
		return (
      <div>
  			<div className="page" dangerouslySetInnerHTML={{__html: this.state.content}} ></div>
        {this.state.isLoading && <Loader />}
      </div>
		);
	}
}

Page.propTypes = {
  page: React.PropTypes.object.isRequired,
  content: React.PropTypes.string.isRequired
};

export default Page;
