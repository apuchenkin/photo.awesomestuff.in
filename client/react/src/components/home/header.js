import React from 'react';
import CategoryLink from '../link/category';
import {FormattedMessage} from 'react-intl';
import Link from '../link';

export default class Header extends React.Component {
	render() {
		return (
			<header className="main" ref="main">
        <h1 className="title"><Link to="/">PHOTO.AWESOMESTUFF.IN</Link><span className="version"> <FormattedMessage
					id="alfa"
					defaultMessage={`alfa`}
					/></span></h1>
        <h2 className="subtitle">
					<FormattedMessage
						id="description"
						defaultMessage={`Travel in photography`}
						/>
        </h2>
      </header>
		);
	}
}
