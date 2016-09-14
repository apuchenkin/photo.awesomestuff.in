import escapeHtml from 'escape-html';
import assets from '../../build/assets.json';

export default function renderHTML({ componentHTML, initialState, meta, config }) {
  return `
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width initial-scale=1.0">
        <title>${escapeHtml(meta.title)}</title>
        <meta name="description" content="${escapeHtml(meta.description)}">
        <meta name="viewport" content="width=device-width">
        <link href='http://fonts.googleapis.com/css?family=Roboto+Condensed:700,300,400' rel='stylesheet' type='text/css'>
        <link rel="stylesheet" href="${config.staticEndpoint}/${assets.main.css}">
        ${meta.links.join('\n')}
    </head>
    <body>
      <div id="react-view" class="wrapper">${componentHTML}</div>
      <script>
        (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
            (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
          m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
        })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

        ga('create', '${escapeHtml(config.analytics)}', 'auto');
        ga('send', 'pageview');
      </script>
      <script type="application/javascript">
        window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};
      </script>
      <script type="application/javascript" src="https://cdn.polyfill.io/v2/polyfill.min.js?features=Array.prototype.find,Array.prototype.findIndex,Intl"></script>
      <script type="application/javascript" src="${config.staticEndpoint}/${assets.main.js}"></script>
    </body>
    </html>
  `;
}
