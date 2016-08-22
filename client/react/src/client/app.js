import React from 'react';
import Router from 'react-router/lib/Router';
import browserHistory from 'react-router/lib/browserHistory';

import ExtraDataProvider from '../lib/provider.js';
import routes         from '../routes';

const initialState = window.__INITIAL_STATE__ || {};

class App extends React.Component {
	render() {
		return (
			<ExtraDataProvider initialState={initialState}>
				<Router history={browserHistory} routes={routes} />
			</ExtraDataProvider>
		);
	}
}

export default App;
