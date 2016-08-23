import React from 'react';
import Link from 'react-router/lib/Link';

class Header extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      page: props.route.page
    }
  }

	render() {
		return (
      <header className="main" ref="main">
				<h1 className="title">
					<Link to='/' activeClassName="active">HOME</Link> / {this.state.page.title}
				</h1>
			</header>
		);
	}
}

// Header.contextTypes = {
//   initialState: React.PropTypes.any.isRequired
// };

export default Header;
