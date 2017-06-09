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
import Translation from './Translation';
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
  }

  componentDidMount() {
    this.fetchCategories();
  }

  fetchCategories() {
    this.categoryService.fetchCategories()
      .then(categories => this.setState({ categories }));
  }

  deletePhotos() {
    const category = this.state.categories.find(c => c.id === this.state.category);

    if (category) {
      this.categoryService.unlinkPhotos(category, this.state.selection)
        .then(() => {
          this.cleanSelection();
          this.fetchPhotos();
        });
    }
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
    const category = match.params.category;

    return (
      <div className="admin">
        <div className="aside">
          <Categories data={categories} admin={this} />
        </div>
        {category && (
          <div className="content">
            <Switch>
              <Route
                exact
                path={match.url}
                render={() => <Photos admin={this} category={category} categories={categories} />}
              />
              <Route path={`${match.url}/translation`} render={() => <Translation categoryName={category} />} />
            </Switch>
          </div>
        )}
      </div>
    );
  }
}

export default withRouter(App);
