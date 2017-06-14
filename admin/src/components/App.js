import React from 'react';
import {
  Route,
  Switch,
  withRouter,
} from 'react-router-dom';

import config from '../../../client/src/etc/config.json';
import PhotoService from '../../../client/lib/service/Photo';
import CategoryService from '../../../client/lib/service/Category';

import Photos from './Photos';
import Translations from './Translations';
import Categories from './Categories';

class App extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      categories: [],
    };

    this.photoService = new PhotoService({
      apiEndpoint: config.apiEndpoint,
    });

    this.categoryService = new CategoryService({
      apiEndpoint: config.apiEndpoint,
    });

    this.fetchCategories = this.fetchCategories.bind(this);
  }

  componentDidMount() {
    this.fetchCategories();
  }

  fetchCategories() {
    this.categoryService.fetchCategories()
      .then(categories => this.setState({ categories }));
  }

  addToCategory(category, photos) {
    this.categoryService.linkPhotos(category, [photos])
      .then(() => {
        this.cleanSelection();
        this.fetchPhotos();
      });
  }

  render() {
    const { categories } = this.state;
    const { match } = this.props;
    const categoryName = match.params.category;
    const category = categories.find(c => c.name === categoryName);
    const photos = <Photos admin={this} category={categoryName} categories={categories} />;
    const translations = <Translations admin={this} category={category} />;

    return (
      <div className="admin">
        <div className="aside">
          <Categories categories={categories} admin={this} />
        </div>
        {category && (
          <main>
            <Switch>
              <Route
                exact
                path={match.url}
                render={() => photos}
              />
              <Route
                path={`${match.url}/translation`}
                render={() => translations}
              />
            </Switch>
          </main>
        )}
      </div>
    );
  }
}

export default withRouter(App);
