import * as React from 'react';
import { __RouterContext } from 'react-router';
import {
  Route,
  Switch,
} from 'react-router-dom';

import Photos from './photos';
import Categories from './categories';
import { CategoryContext } from '@app/context';
import CategoryTranslations from './translation/category';

const Category = () => {
  const { match } = React.useContext(__RouterContext);
  const { getCategory } = React.useContext(CategoryContext);

  // @ts-ignore
  const category = getCategory(match.params.category);

  return (
    <main>
      {category && (
        <Switch>
          <Route
            key={category.name}
            path={`${match.url}/photo`}
            render={() => <Photos category={category} />}
          />
          <Route
            path={`${match.url}/translation`}
            component={() => <CategoryTranslations category={category} backUrl={match.url} />}
          />
        </Switch>
      )}
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
