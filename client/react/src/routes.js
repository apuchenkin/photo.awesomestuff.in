import React                     from 'react';
import { Route, Redirect, createRoutes, withRouter, IndexRoute } from 'react-router';
import CategoryService from './service/Category';
import PhotoService from './service/Photo';
import Home from './components/home';
import HomeHeader from './components/home/header';
import Gallery from './components/gallery';
import GalleryHeader from './components/gallery/header';
import Photo from './components/photo';
import Link from 'react-router/lib/Link';

import './style/main.less';

const categoryService = new CategoryService();
const photoService = new PhotoService();

class App extends React.Component {

    constructor(props, context) {
	    super(props, context);

	    this.state = {
				category: props.params ? props.params.category : null,
				categories: context.initialState ? context.initialState.categories || [] : []
			}
	  }

  	componentDidMount() {
  		let me = this;

  		me.props.route.resolve(me.props.params).categories
  			.then(categories => me.setState({categories: categories}));
  	}

    render() {
      const
        categories = this.state.categories,
        header = categories && categories.length && this.props.header && React.cloneElement(this.props.header, {
          categories: categories
        }),
        childrens = categories && categories.length && React.Children.map(this.props.children, c => React.cloneElement(c, {
          categories: categories
        }));

      return (
        <div id="main" className="home">
          {header}
          {this.props.body}
          {childrens}
          <footer>
            <Link to="/">photo.awesomestuff.in</Link> | © 2015, Пученкин Артём | <Link to="/about">О сайте</Link> | <Link to="/contacts">Контакты</Link>
          </footer>
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
    <Route path="/" component={App} resolve={params => ({
        categories: categoryService.fetchCategories()
      })}
      > //(:locale)
      <IndexRoute components={{header: HomeHeader, body: Home}} />
      <Route path=":category(/:subcategory)"
        components={{header: GalleryHeader, body: Gallery}}
        resolve={params => ({
      			photos: photoService.fetchPhotos(params.subcategory || params.category).then(p =>
      				photoService.remapPhotos(
      					photoService.refinePhotos(p, params.photoId)
      				)
      			)
    		})}
        >
        <Route path="/:category(/:subcategory)/photo/:photoId"
          component={Photo}
          resolve={params => ({
            photo: photoService.fetchPhoto(params.photoId)
          })}
          />
      </Route>
    </Route>
  //   <Route path="*" component={NoMatch} />
  // </Route>
);
