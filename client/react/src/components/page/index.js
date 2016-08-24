import React from 'react';
import Link from 'react-router/lib/Link';

class Page extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      page: props.page || {}
    }
  }

  // componentDidMount() {
  //   console.log('componentDidMount', this.state)
  // }

	render() {
		return (
			<div className="page" dangerouslySetInnerHTML={{__html: this.state.page.content}} ></div>
		);
	}
}

// Page.contextTypes = {
//   initialState: React.PropTypes.object.isRequired
// };

export default Page;
