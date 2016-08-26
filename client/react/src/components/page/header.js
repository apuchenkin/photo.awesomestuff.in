import React from 'react';
import Link from 'react-router/lib/Link';

class Header extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      page: props.page
    }
  }

  componentWillReceiveProps(props) {
    this.setState({page: props.page});
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

Header.propTypes = {
  page: React.PropTypes.object.isRequired
};

export default Header;
