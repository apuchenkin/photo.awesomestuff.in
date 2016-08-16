import { h, Component } from 'preact';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';

import Header from './header';
import Home from './home';
import Profile from './profile';


function log () {
	console.log(arguments);
}

const app = class App extends Component {
	/** Gets fired when the route changes.
	 *	@param {Object} event		"change" event from [preact-router](http://git.io/preact-router)
	 *	@param {string} event.url	The newly routed URL
	 */
	handleRoute() {
		if (!this.state.params.locale) {
			let lang = navigator.userLanguage || navigator.language
			this.router.replace('/' + lang);
		}
	};

	render() {
		return (
			<div id="app">
				<Header />
				<Router history={browserHistory} onUpdate={this.handleRoute} >
					<Route path="/(:locale)" component={Home} >
						<Route path="/:category(/:subcategory)" component={Profile} />
					</Route>
				</Router>
			</div>
		);
	}
}

export default app;
