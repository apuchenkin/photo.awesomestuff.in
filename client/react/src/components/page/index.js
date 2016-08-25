import React from 'react';
import Link from 'react-router/lib/Link';

class Page extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
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

    debugger;
    if (props.page.id != state.page.id) {
      props.route.parent.resolve(props.params).then(data => {
        this.setState(Object.assign(data, {
          content: data.page.content,
        }))
      })
    }
  }

	render() {
		return (
			<div className="page" dangerouslySetInnerHTML={{__html: this.state.content}} ></div>
		);
	}
}

Page.propTypes = {
  page: React.PropTypes.object.isRequired,
  content: React.PropTypes.string.isRequired
};

export default Page;
