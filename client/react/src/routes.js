import React                     from 'react';
import { Route, Redirect, createRoutes, withRouter, IndexRoute } from 'react-router';
import CategoryService from './service/Category';
import Home from './components/home';
import Gallery from './components/gallery';
import Photo from './components/photo';

class App extends React.Component {

    constructor(props, context) {
	    super(props, context);

	    this.state = {
				category: props.params ? props.params.category : null,
				categories: context.initialState ? context.initialState.categories || [] : [],
				photos: [],
				groups: [],
				showHidden: false
			}
	  }

  	static fetchData (location) {
      let categoryService = new CategoryService(null, location);

      return {
  			categories: categoryService.fetchCategories()
  		}
    }

  	componentDidMount() {
  		let me = this;
  		App.fetchData(location.origin).categories
  			.then(categories => me.setState({categories: categories}));
  	}

    render() {
      return (
          <div id='app-view'>
              {this.props.children}
          </div>
      );
    }
}

App.contextTypes = {
  initialState: React.PropTypes.any.isRequired
};

class NoMatch extends React.Component {
  render() {
    return (
      <div>NoMatch</div>
    )
  }
}

export default (
  // <Route component={withRouter(App)}>
    // <Redirect from='' to='/' />
    <Route path="/" component={App} > //(:locale)
      <IndexRoute component={Home} />
      <Route path=":category(/:subcategory)" component={Gallery} >
        <Route path="photo/:photoId" component={Photo} />
      </Route>
    </Route>
  //   <Route path="*" component={NoMatch} />
  // </Route>
);
