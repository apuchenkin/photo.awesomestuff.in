/* eslint-disable react/no-danger */
import React from 'react';
import { string, shape, object } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { Helmet } from 'react-helmet';
import assets from '../../dist/assets.json';
import favicon from '../assets/favicon.ico';
import style from '../style/style.less';
import localeData from './localeData';

const GoogleAnalytics = ({ token }) => (
  <script
    dangerouslySetInnerHTML={{
      __html: `
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', '${token}');
      `,
    }}
  />
);

GoogleAnalytics.propTypes = {
  token: string.isRequired,
};

function renderHTML({ markup, initialState, styles }) {
  const { config, locale } = initialState.runtime;
  const helmet = Helmet.renderStatic();
  const htmlAttrs = helmet.htmlAttributes.toComponent();
  const bodyAttrs = helmet.bodyAttributes.toComponent();
  const analyticsToken = config.analytics;

  return (
    <html lang={locale} {...htmlAttrs}>
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width initial-scale=1.0" />
        {helmet.title.toComponent()}
        {helmet.meta.toComponent()}
        <meta name="viewport" content="width=device-width" />
        <style type="text/css" dangerouslySetInnerHTML={{ __html: styles }} />
        <link rel="shortcut icon" href={favicon} type="image/ico" />
        {helmet.link.toComponent()}
        {analyticsToken && <script async src={`https://www.googletagmanager.com/gtag/js?id=${analyticsToken}`} />}
        {analyticsToken && <GoogleAnalytics token={analyticsToken} />}
      </head>
      <body {...bodyAttrs}>
        <div
          id="react-view"
          className={style.wrapper}
          dangerouslySetInnerHTML={{ __html: markup }}
        />
        <script
          dangerouslySetInnerHTML={{ __html: localeData(locale) }}
        />
        <script type="application/javascript" dangerouslySetInnerHTML={{ __html: `window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};` }} />
        <script type="application/javascript" src={`${config.hostname}${assets.main.js}`} />
        <link href="http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400" rel="stylesheet" type="text/css" />
      </body>
    </html>
  );
}

renderHTML.propTypes = {
  markup: string.isRequired,
  initialState: shape({
    runtime: shape({
      locale: string.isRequired,
      config: object.isRequired,
    }),
  }).isRequired,
  styles: string.isRequired,
};

export default withStyles(style)(renderHTML);
