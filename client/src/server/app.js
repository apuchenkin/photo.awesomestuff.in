import React from 'react';
import { Provider } from 'react-redux';
import ReactDOM from 'react-dom/server';
// import match from 'react-router/lib/match';
// import RouterContext from 'react-router/lib/RouterContext';

import { Actions as FarceActions, ServerProtocol } from 'farce';
import { getStoreRenderArgs, resolver, RedirectException } from 'found';
import { RouterProvider } from 'found/lib/server';

import express from 'express';
import { sync as globSync } from 'glob';
import { IntlProvider } from 'react-intl';
import path from 'path';
import { readFileSync } from 'fs';

import HTML from './renderHTML';
import configureStore from '../store/configureStore';
import render from './render';
// import createRoutes from '../routes';
import config from '../etc/config.json';
// import utils from '../lib/utils';
import WithStylesContext from '../components/WithStylesContext';

const translations = globSync('./translation/*.json')
  .map(filename => [
    path.basename(filename, '.json'),
    readFileSync(filename, 'utf8'),
  ])
  .map(([locale, file]) => [locale, JSON.parse(file)])
  .reduce((collection, [locale, messages]) =>
    Object.assign(collection, { [locale]: messages }),
    {},
  );

const app = express();

const negotiateLocale = req =>
  req.acceptsLanguages(config.locales)
  || config.fallbackLocale
;

app.use(express.static(path.join(__dirname, 'assets')));
app.use(async (req, res) => {
  const piece = req.url.split('/')[1];
  const prefix = config.locales.find(l => l === piece);
  const basename = prefix && `/${prefix}`;
  const locale = prefix || negotiateLocale(req);
  const messages = locale ? translations[locale] : {};
  const initial = { runtime: { locale, basename, messages, config } };
    // routes = createRoutes(store),
  // const location = basename ? req.url.replace(basename, '') || '/' : req.url;
  const css = new Set(); // CSS for all rendered React components
    // eslint-disable-next-line no-underscore-dangle
  const onInsertCss = (...styles) => styles.forEach(style => css.add(style._getCss()));
  const store = await configureStore(new ServerProtocol(req.url), initial);
  store.dispatch(FarceActions.init());

  const matchContext = { store };
  let renderArgs;

  try {
    renderArgs = await getStoreRenderArgs({
      store,
      matchContext,
      resolver,
    });
  } catch (e) {
    if (e instanceof RedirectException) {
      res.redirect(302, store.farce.createHref(e.location));
      return;
    }

    throw e;
  }

  if (!basename) {
    res.vary('Accept-Language');
  }

  const markup = ReactDOM.renderToString(
    <Provider store={store}>
      <IntlProvider locale={locale} messages={messages}>
        <WithStylesContext onInsertCss={onInsertCss}>
          <RouterProvider router={renderArgs.router}>
            {render(renderArgs)}
          </RouterProvider>
        </WithStylesContext>
      </IntlProvider>
    </Provider>,
  );

  // const meta = utils.getMeta(renderProps.routes, messages, renderProps.location.pathname),
  const meta = {};
  const styles = [...css].join('');
  const html = ReactDOM.renderToStaticMarkup(
    <WithStylesContext onInsertCss={onInsertCss}>
      <HTML
        markup={markup}
        initialState={store.getState()}
        meta={meta}
        styles={styles}
      />
    </WithStylesContext>,
  );

  res
    .status(renderArgs.error ? renderArgs.error.status : 200)
    .send(`
      <!DOCTYPE html>
      ${html}
    `);


  // // Note that req.url here should be the full URL path from
  // // the original request, including the query string.
  // match({ routes, location, basename }, (error, redirectLocation, renderProps) => {
  //   if (error) {
  //     // TODO: 503 error
  //     res.status(500).send(error.message);
  //   } else if (redirectLocation) {
  //     // moved permanently
  //     res.redirect(301, (basename || '') + redirectLocation.pathname + redirectLocation.search);
  //   } else if (renderProps) {
  //     const
  //       meta = utils.getMeta(renderProps.routes, messages, renderProps.location.pathname),
  //       componentHTML = ReactDOM.renderToString(
  //         <Provider store={store}>
  //           <IntlProvider locale={locale} messages={messages}>
  //             <WithStylesContext onInsertCss={onInsertCss}>
  //               <RouterContext {...renderProps} />
  //             </WithStylesContext>
  //           </IntlProvider>
  //         </Provider>,
  //       ),
  //       styles = [...css].join(''),
  //       html = ReactDOM.renderToStaticMarkup(
  //         <WithStylesContext onInsertCss={onInsertCss}>
  //           <HTML
  //             componentHTML={componentHTML}
  //             initialState={store.getState()}
  //             meta={meta}
  //             styles={styles}
  //           />
  //         </WithStylesContext>,
  //       );
  //
  //     res.status(200).send(`<!DOCTYPE html>${html}`);
  //
  //     // You can also check renderProps.components or renderProps.routes for
  //     // your "not found" component or route respectively, and send a 404 as
  //     // below, if you're using a catch-all route.
  //   } else {
  //     // TODO: 404 page should have proper status and content page
  //     res.status(404).send('Not found');
  //   }
  // });
});

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
