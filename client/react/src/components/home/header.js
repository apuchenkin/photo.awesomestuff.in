import React from 'react';
import CategoryLink from '../link/category';
import Link from 'react-router/lib/Link';

class Header extends React.Component {
	render() {
		return (
			<header className="main" ref="main">
        <h1 className="title"><Link to="/">PHOTO.AWESOMESTUFF.IN</Link><span className="version"> альфа</span></h1>
        <h2 className="subtitle">Путешествия в фотографиях</h2>
      </header>
		);
	}
}

export default Header
