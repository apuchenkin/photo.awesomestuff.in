// import Redirect from 'found/lib/Redirect';
import Main from './components/main';
import HomeHeader from './components/home/header';
import Home from './components/home';
import PageHeader from './components/page/header';
import Page from './components/page';
import { load as loadCategories } from './store/category/actions';
import { load as loadPage } from './store/page/actions';

export default pages => [
  {
    path: '/',
    Component: Main,
    getData: ({ routes }) => ({
      className: 'home',
      header: routes[routes.length - 1].header,
    }),
    children: [
      {
        data: {
          className: 'home-main',
        },
        header: HomeHeader,
        Component: Home,
        getData: ({ routes, context: { store } }) => {
          const categories = new Promise((resolve, reject) => {
            store.dispatch(loadCategories(resolve, reject));
          });

          return categories.then(() => ({
            className: 'home-main',
            header: routes[routes.length - 1].header,
          }));
        },
      },
      ...pages.map(page => ({
        path: page.alias,
        header: PageHeader,
        Component: Page,
        getData: ({ context: { store } }) => {
          const promise = new Promise((resolve, reject) => {
            store.dispatch(loadPage(page, resolve, reject));
          });

          return promise.then(page$ => ({
            meta: {
              title: page$.title,
              description: page$.description, // TODO: create page description on BE
            },
            langs: page$.langs,
          }));
        },
      })),
      // {
      //   path: '/:category',
      //   header: HomeHeader,
      //   getData: ({ params, routes, context: { store } }) => {
      //     console.log(params);
      //     console.log(3);
      //     debugger;
      //
      //     return {};
      //   },
      //   getComponent: () => {
      //     console.log(4);
      //     debugger;
      //
      //     return HomeHeader;
      //   },
      // },
    ],
  },
];
