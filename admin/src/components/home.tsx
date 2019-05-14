import * as React from 'react';
import {
  Route,
  Switch,
  // withRouter,
} from 'react-router-dom';

// import Photos from './Photos';
// import CategoryTranslations from './translation/Category';
import Categories from './categories';
import { CategoryContext } from '@app/context';

const App = () => {
  // const { match, categories } = this.props;
  // const categoryName = match.params.category;
  const categories = React.useContext(CategoryContext);
  // const category = categories.find(c => c.name === categoryName);
  // const photos = <Photos admin={this} category={category} />;
  // const transalations = (
  //   <CategoryTranslations backUrl={match.url} category={category} />
  // );

  return (
    <div className="admin">
      <div className="aside">
        <Categories categories={categories} />
      </div>
      {/* {category && (
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
      )} */}
    </div>
  );
}

export default App;
