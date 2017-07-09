/* eslint-disable react/no-danger */
import React from 'react';
import { string, shape, array, object } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import escapeHtml from 'escape-html';
import assets from '../../dist/assets.json';
import favicon from '../assets/favicon.ico';
import style from '../style/style.less';

const GoogleAnalytics = ({ id }) => (
  <script
    dangerouslySetInnerHTML={{ __html: `
      (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
      })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
      ga('create', '${id}', 'auto'); ga('send', 'pageview');`,
    }}
  />
);

GoogleAnalytics.propTypes = {
  id: string.isRequired,
};

function renderHTML({ markup, initialState, meta, styles }) {
  const { config, locale } = initialState.runtime;

  return (
    <html lang={locale}>
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width initial-scale=1.0" />
        <title>{escapeHtml(meta.title)}</title>
        <meta name="description" content={escapeHtml(meta.description)} />
        <meta name="viewport" content="width=device-width" />
        <link href="http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400" rel="stylesheet" type="text/css" />
        <style type="text/css" dangerouslySetInnerHTML={{ __html: styles }} />
        <link rel="shortcut icon" href={favicon} type="image/ico" />
        { meta.links }
      </head>
      <body>
        <div
          id="react-view"
          className={style.wrapper}
          dangerouslySetInnerHTML={{ __html: markup }}
        />
        {config.analytics && <GoogleAnalytics id={config.analytics} />}
        <script type="application/javascript" dangerouslySetInnerHTML={{ __html: `window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};` }} />
        <script type="application/javascript" src={`${config.hostname}${assets.main.js}`} />
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
  meta: shape({
    title: string.isRequired,
    description: string.isRequired,
    links: array.isRequired,
  }).isRequired,
  styles: string.isRequired,
};

export default withStyles(style)(renderHTML);
