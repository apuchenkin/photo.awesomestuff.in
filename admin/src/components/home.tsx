import * as React from 'react';
import { __RouterContext } from 'react-router';
import {
  Route,
  Switch,
} from 'react-router-dom';

import Photos from './photos';
// import CategoryTranslations from './translation/category';
import Categories from './categories';
import { CategoryContext } from '@app/context';

const Category = () => {
  const { match } = React.useContext(__RouterContext);
  const { getCategory } = React.useContext(CategoryContext);
  const category = getCategory(match.params.category);

  return (
    <main>
      <Switch>
        {category && (
          <Route
            path={`${match.url}/photo`}
            render={() => <Photos category={category} />}
          />
        )}

        {/* <Route
          path={`${match.url}/translation`}
          component={CategoryTranslations}
        /> */}
      </Switch>
    </main>
  )
};

const Home = () => {
  const { getCategories } = React.useContext(CategoryContext);

  return (
    <div className="admin">
      <div className="aside">
        <Categories categories={getCategories()} />
      </div>
      <Switch>
        <Route path="/category/:category" component={Category} />
      </Switch>
    </div>
  );
}

export default Home;
