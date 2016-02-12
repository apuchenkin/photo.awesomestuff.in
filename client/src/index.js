'use strict';

require('./index.html');
var Elm = require('./Main');

var Main = Elm.embed(Elm.Main, document.getElementById('main'), {localePort: navigator.language});

Main.ports.meta.subscribe(metaUpdate);

function metaUpdate(meta) {
  document.title = meta.title;
}
