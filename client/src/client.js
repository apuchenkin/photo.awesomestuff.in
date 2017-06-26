import React from 'react';
import { Provider } from 'react-redux';
import ReactDOM from 'react-dom';
import { BrowserProtocol } from 'farce';
import createConnectedRouter from 'found/lib/createConnectedRouter';
import getStoreRenderArgs from 'found/lib/getStoreRenderArgs';
import resolver from 'found/lib/resolver';
import { IntlProvider, addLocaleData } from 'react-intl';
import ruLocaleData from 'react-intl/locale-data/ru';
import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

import configureStore from './store/configureStore';
// import createRoutes from './routes';
// import utils from './lib/utils';
import WithStylesContext from './components/WithStylesContext';
import render from './server/render';

import './style/style.css';

addLocaleData(ruLocaleData);

// const span = document.createElement('span');
const initialState = isBrowser && (window.__INITIAL_STATE__ || {});
const ConnectedRouter = createConnectedRouter({ render });

// function metaUpdate(meta) {
//   document.title = meta.title;
//   document.head.querySelector('meta[name=description]').content = meta.description;
//   Array.from(document.head.querySelectorAll('link[hreflang]')).map((node) => {
//     ReactDOM.unmountComponentAtNode(node);
//     document.head.removeChild(node);
//     return false;
//   });
//   const links = meta.links.reduce((acc, link) => {
//     ReactDOM.render(link, span);
//     return acc.concat(span.innerHTML);
//   }, []);
//   document.head.insertAdjacentHTML('beforeend', links.join('\n'));
// }

// function onUpdate() {
//   const
//     { routes, location } = this.state,
//     meta = utils.getMeta(routes, messages, location.pathname);
//
//   metaUpdate(meta);
//
//   if (typeof ga !== 'undefined') {
//     ga('send', 'pageview', {
//       title: meta.title,
//       page: location.pathname,
//     });
//   }
// }

function onInsertCss(...styles) {
  // eslint-disable-next-line no-underscore-dangle, max-len
  const removeCss = styles.map(style => style._insertCss());
  return () => {
    removeCss.forEach(f => f());
  };
}

(async () => {
  const store = await configureStore(new BrowserProtocol(), initialState);
  const matchContext = { store };
  const { locale, basename, messages } = store.getState().runtime;

  const initialRenderArgs = await getStoreRenderArgs({
    store,
    matchContext,
    resolver,
  });

  ReactDOM.render(
    <Provider store={store}>
      <IntlProvider locale={locale} messages={messages}>
        <WithStylesContext onInsertCss={onInsertCss}>
          <ConnectedRouter
            matchContext={matchContext}
            resolver={resolver}
            initialRenderArgs={initialRenderArgs}
          />
        </WithStylesContext>
      </IntlProvider>
    </Provider>,
    document.getElementById('react-view'),
  );
})();
