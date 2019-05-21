import * as React from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles'
import assets from '@assets/assets.json';
import favicon from '@assets/favicon.ico';
import { Helmet } from 'react-helmet';
import localeData from './localeData';
import { values, mapObjIndexed } from 'ramda';
import style from '../style.scss';

interface AnalyticsProps {
  token: string;
}

const GoogleAnalytics: React.FunctionComponent<AnalyticsProps> = ({ token }) => (
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

interface Props {
  markup: string;
  initialState: any;
  css: Set<string>;
}

const renderHTML: React.FunctionComponent<Props> = ({ markup, initialState, css }) => {
  const { config, locale } = initialState;
  const helmet = Helmet.renderStatic();
  const htmlAttrs = helmet.htmlAttributes.toComponent();
  const bodyAttrs = helmet.bodyAttributes.toComponent();
  const analyticsToken = config.analytics;
  const styles = [...css].join('\n');

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
        <script
          dangerouslySetInnerHTML={{ __html: localeData(locale) }}
        />
        <script type="application/javascript" dangerouslySetInnerHTML={{ __html: `window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};` }} />
      </head>
      <body {...bodyAttrs}>
        <div
          id="react-root"
          className={style.wrapper}
          dangerouslySetInnerHTML={{ __html: markup }}
        />
        {config.analytics && <GoogleAnalytics token={config.analytics} />}
        {values(mapObjIndexed((asset: any, index) => (
          <React.Fragment key={index}>
            {asset.js && (
              <script src={`${asset.js}`} type="application/javascript" />
            )}
            {asset.css && (
              <link href={`${asset.css}`} rel="stylesheet" type="text/css" />
            )}
          </React.Fragment>
        ), assets))}
        <link href="http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400" rel="stylesheet" type="text/css" />
      </body>
    </html>
  );
}

export default withStyles(style)(renderHTML);
