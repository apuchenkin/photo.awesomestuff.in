import React                     from 'react';
import { Route, Redirect, createRoutes, withRouter, IndexRoute } from 'react-router';
import CategoryService from './service/Category';
import Home from './components/home';
import Gallery from './components/gallery';
import Photo from './components/photo';
import './style/main.less';

class App extends React.Component {

    constructor(props, context) {
	    super(props, context);

	    this.state = {
				category: props.params ? props.params.category : null,
				categories: context.initialState ? context.initialState.categories || [] : []
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
      const
        categories = this.state.categories,
        childrens = categories && categories.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
          categories: categories
        }));

      return (
        <div id='app-view'>
          {childrens}
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
