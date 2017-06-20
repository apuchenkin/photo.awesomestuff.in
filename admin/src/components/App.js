import React from 'react';
import {
  Route,
  Switch,
  withRouter,
} from 'react-router-dom';
import { connect } from 'react-redux';

import Photos from './Photos';
import CategoryTranslations from './translation/Category';
import Categories from './Categories';

import { load as loadCategories } from '../store/category/actions';

class App extends React.PureComponent {
  componentDidMount() {
    this.props.loadCategories();
  }

  render() {
    const { match, categories } = this.props;
    const categoryName = match.params.category;
    const category = categories.find(c => c.name === categoryName);
    const photos = <Photos admin={this} category={category} />;
    const transalations = (
      <CategoryTranslations backUrl={match.url} category={category} />
    );

    return (
      <div className="admin">
        <div className="aside">
          <Categories categories={categories} admin={this} />
        </div>
        {category && (
          <main>
            <Switch>
              <Route
                path={`${match.url}/photo`}
                render={() => photos}
              />
              <Route
                path={`${match.url}/translation`}
                render={() => transalations}
              />
            </Switch>
          </main>
        )}
      </div>
    );
  }
}

export default withRouter(connect(
  ({ category: { categories } }) => ({
    categories,
  }),
  dispatch => ({
    loadCategories: () => dispatch(loadCategories()),
  }),
)(App));
