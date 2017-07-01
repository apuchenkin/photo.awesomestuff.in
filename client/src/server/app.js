import path from 'path';
import React from 'react';
import { Provider } from 'react-redux';
import ReactDOM from 'react-dom/server';
import { Actions as FarceActions, ServerProtocol } from 'farce';
import { getStoreRenderArgs, resolver, RedirectException } from 'found';
import { RouterProvider } from 'found/lib/server';

import express from 'express';
import { sync as globSync } from 'glob';
import { IntlProvider } from 'react-intl';
import { readFileSync } from 'fs';

import HTML from './renderHTML';
import configureStore from '../store/configureStore';
import { serverRender as render } from '../render';
import config from '../etc/config.json';
import { buildMeta } from '../lib/meta';
import WithStylesContext from '../components/WithStylesContext';

const translations = globSync(path.join(process.cwd(), 'src', 'translation', '*.json'))
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
  const css = new Set(); // CSS for all rendered React components
    // eslint-disable-next-line no-underscore-dangle
  const onInsertCss = (...styles) => styles.forEach(style => css.add(style._getCss()));
  const intlProvider = new IntlProvider({
    locale,
    messages,
  }, {});
  const { intl } = intlProvider.getChildContext();
  const store = await configureStore(new ServerProtocol(req.url), initial, intl);
  store.dispatch(FarceActions.init());

  const matchContext = { store, intl };
  let renderArgs;

  try {
    renderArgs = await getStoreRenderArgs({
      store,
      matchContext,
      resolver,
    });
  } catch (e) {
    if (e instanceof RedirectException) {
      res.redirect(301, store.farce.createHref(e.location));
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

  const meta = buildMeta(renderArgs.location, store, intl);
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
});

const PORT = process.env.PORT || 3001;

app.listen(PORT, () => {
  // eslint-disable-next-line no-console
  console.log(`Server listening on: ${PORT}`);
});
