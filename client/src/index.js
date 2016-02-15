'use strict';

require('./index.html');
var Elm = require('./Main');
var css = require("!style!css!less!../assets/styles/main.less");

var Main = Elm.embed(Elm.Main, document.getElementById('main'), {localePort: navigator.language});

Main.ports.meta.subscribe(metaUpdate);

function metaUpdate(meta) {
  document.title = meta.title;
  meta.links.map(function(data) {
    var link = document.createElement('link');
    link.href = data[1];
    link.rel = "alternate";
    link.hreflang = data[0];
    document.head.appendChild(link);
  })
}
