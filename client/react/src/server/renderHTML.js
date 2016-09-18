/* eslint-disable react/no-danger */
import React from 'react';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import escapeHtml from 'escape-html';
import assets from '../../build/assets.json';
import config from '../config/config.json';

import style from '../style/style.less';

const { string, shape, array } = React.PropTypes;

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
  id: React.PropTypes.string.isRequired,
};

function renderHTML({ componentHTML, initialState, meta, styles }) {
  return (
    <html lang={initialState.locale}>
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width initial-scale=1.0" />
        <title>{escapeHtml(meta.title)}</title>
        <meta name="description" content={escapeHtml(meta.description)} />
        <meta name="viewport" content="width=device-width" />
        <link href="http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400" rel="stylesheet" type="text/css" />
        <style type="text/css" dangerouslySetInnerHTML={{ __html: styles }} />
        { meta.links }
      </head>
      <body>
        <div id="react-view" className={style.wrapper} dangerouslySetInnerHTML={{ __html: componentHTML }} />
        {config.analytics && <GoogleAnalytics id={config.analytics} />}
        <script type="application/javascript" dangerouslySetInnerHTML={{ __html: `window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};` }} />
        <script type="application/javascript" src={`${config.staticEndpoint}${assets.main.js}`} />
      </body>
    </html>
  );
}

renderHTML.propTypes = {
  componentHTML: string.isRequired,
  initialState: shape({
    locale: string.isRequired,
  }).isRequired,
  meta: shape({
    title: string.isRequired,
    description: string.isRequired,
    links: array.isRequired,
  }).isRequired,
  styles: string.isRequired,
};

export default withStyles(style)(renderHTML);
