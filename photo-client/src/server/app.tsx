import * as React from 'react';
import * as R from 'ramda';
import * as ReactDOM from 'react-dom/server';
// @ts-ignore
import { getFarceResult } from 'found/lib/server';
import express from 'express';
import { IntlProvider } from 'react-intl';
import glob from 'glob';
import path from 'path';
import { readFileSync } from 'fs';

import HTML from './renderHTML';
import routeConfigFactory from '../router/config';
import render from '../router/render';
import { RequestHandler } from 'express-serve-static-core';
// @ts-ignore
import StyleContext from 'isomorphic-style-loader/StyleContext';
import { ConfigProvider, PageProvider } from '@app/context';
import serviceFactory from '../service/factory';

// @ts-ignore
import { queryMiddleware, createBasenameMiddleware } from 'farce';

// @ts-ignore
import config from '../../etc/config';

interface Translations {
  [key: string]: {}
}

const translations: Translations = glob.sync(
  path.join(process.cwd(), 'translation', '*.json'))
  .map(filename => [
    path.basename(filename, '.json'),
    readFileSync(filename, 'utf8'),
  ])
  .map(([locale, file]) => [locale, JSON.parse(file)])
  .reduce(
    (collection, [locale, messages]) => ({
      ...collection,
      [locale]: messages,
    }),
    {}
  );

function catchAsyncErrors(fn: RequestHandler): RequestHandler {
  return (req, res, next) => {
    const routePromise = fn(req, res, next);
    if (routePromise.catch) {
      routePromise.catch((err: Error) => next(err));
    }
  };
}

const negotiateLocale = (req: express.Request) =>
  req.acceptsLanguages(config.locales)
  || config.fallbackLocale
;

export default () => {
  const app = express();

  app.use(catchAsyncErrors(async (req, res) => {
    const piece = req.url.split('/')[1];
    const prefix = R.find(R.equals(piece), config.locales);
    const basename = prefix && `/${prefix}`;
    const locale = prefix || negotiateLocale(req);
    const messages = locale ? translations[locale] : {};
    const css = new Set(); // CSS for all rendered React components
    const insertCss = (...styles: any) => styles.forEach((style: any) => {
      css.add(style._getCss())
    })

    const services = serviceFactory({
      locale,
      endpoint: process.env.API_ENDPOINT || `${config.hostname}${config.apiEndpoint}`,
    });

    const pages = await services
      .pageService
      .fetchPages()
      .catch((e: Error) => {
        console.error(e);
        return [];
      });

    const categories = await services
      .categoryService
      .fetchCategories()
      .catch((e: Error) => {
        console.error(e);
        return [];
      });

    const routeConfig = routeConfigFactory({
      pages,
      categories,
      services,
    })

    const matchContext: any = {};
    const { redirect, status, element } = await getFarceResult({
      url: req.url,
      historyMiddlewares: [
        queryMiddleware,
        createBasenameMiddleware({ basename }),
      ],
      routeConfig,
      matchContext,
      render,
    });

    const initial = {
      config,
      locale,
      basename,
      messages,
      pages,
      categories,
      data: matchContext.data,
    };

    if (redirect) {
      res.redirect(302, redirect.url);
      return;
    }

    if (!basename) {
      res.vary('Accept-Language');
    }

    const markup = ReactDOM.renderToString(
      <IntlProvider locale={locale} messages={messages}>
        <StyleContext.Provider value={{ insertCss }}>
          <ConfigProvider config={config}>
            <PageProvider pages={pages}>
              {element}
            </PageProvider>
          </ConfigProvider>
        </StyleContext.Provider>
      </IntlProvider>
    );

    const html = ReactDOM.renderToStaticMarkup(
      <StyleContext.Provider value={{ insertCss }}>
        <HTML
          markup={markup}
          initialState={initial}
          css={css}
        />
      </StyleContext.Provider>,
    );

    res
      .status(status)
      .send(`
        <!DOCTYPE html>
        ${html}
      `);
  }));

  return app;
}

