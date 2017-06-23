import Main from './components/main';
import HomeHeader from './components/home/header';
import Home from './components/home';
import { load as loadCategories } from './store/category/actions';

export default [
  {
    path: '/',
    Component: Main,
    getData: ({ routes, context: { store } }) => {
      store.dispatch(loadCategories());

      return {
        className: 'home',
        header: routes[routes.length - 1].header,
      };
    },
    children: [
      {
        data: {
          className: 'home-main',
        },
        header: HomeHeader,
        Component: Home,
      },
    ],
  },
];
