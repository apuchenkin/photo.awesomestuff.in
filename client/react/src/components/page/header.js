import React from 'react';
import {FormattedMessage} from 'react-intl';
import Link from '../link';

class Header extends React.Component {
  componentWillReceiveProps(props) {
    this.setState({page: props.page});
  }

	render() {
		return (
      <header className="main" ref="main">
				<h1 className="title">
					<Link to='/' activeClassName="active"><FormattedMessage
						id="home"
						defaultMessage={`Home`}
						/></Link> / {this.props.page.title}
				</h1>
			</header>
		);
	}
}

Header.propTypes = {
  page: React.PropTypes.object.isRequired
};

export default Header;
