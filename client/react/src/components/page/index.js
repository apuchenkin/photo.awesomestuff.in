import React from 'react';
import Link from 'react-router/lib/Link';

class Page extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      page: props.route.page
    }
  }

	render() {
		return (
			<div class="page" dangerouslySetInnerHTML={{__html: this.state.page.content}} ></div>
		);
	}
}

// Page.contextTypes = {
//   initialState: React.PropTypes.any.isRequired
// };

export default Page;
