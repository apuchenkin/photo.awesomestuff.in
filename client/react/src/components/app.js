import React from 'react';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter, match} from 'react-router';
import ExtraDataProvider from './provider.js';
import routes         from '../routes';

function log () {
	console.log(arguments);
}

const initialState = window.__INITIAL_STATE__ || {};

class App extends React.Component {
	/** Gets fired when the route changes.
	 *	@param {string} event.url	The newly routed URL
	 */
	handleRoute() {
		// if (!this.state.params.locale) {
		// 	let lang = navigator.userLanguage || navigator.language
		// 	this.router.replace('/' + lang);
		// }
	};

	render() {
		return (
			<ExtraDataProvider initialState={initialState}>
				<Router history={browserHistory} onUpdate={this.handleRoute} routes={routes} />
			</ExtraDataProvider>
		);
	}
}

export default App;
