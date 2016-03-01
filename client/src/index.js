'use strict';

require('./index.html');
var Elm = require('./Main');
var css = require("../assets/styles/main.less");
var fontello = require('../assets/fontello/css/fontello.css');
var Ps = require('perfect-scrollbar');
var pscss = require('perfect-scrollbar/dist/css/perfect-scrollbar.css');

var Main = Elm.fullscreen(Elm.Main, {
  localePort: navigator.language,
  timePort: Date.now()
});

Main.ports.meta.subscribe(metaUpdate);
Main.ports.rs.subscribe(onTransition);

var main = document.body.querySelector('#main');
var links = {};

function metaUpdate(meta) {
  document.title = meta.title;
  meta.links.map(function(data) {
    var link = links[data[0]] || document.head.appendChild(document.createElement('link'));
    link.href = data[1];
    link.rel = "alternate";
    link.hreflang = data[0];
    links[data[0]] = link;
  })
}

Ps.initialize(main.querySelector('.content'));

var headerElm = main.querySelector('header');
var headerObserver = new MutationObserver(function(mutations) {
  main.setAttribute("style", "padding-top: " + headerElm.offsetHeight + "px;");
});
main.setAttribute("style", "padding-top: " + headerElm.offsetHeight + "px;");
headerObserver.observe(headerElm, {childList: true});

var packery = null;

function onTransition(args) {
  // configuration of the observer:
  var grid = main.querySelector('.gallery');
  if (!packery && grid) {
    require.ensure([], function() {
      var Packery = require('packery');
      packery = new Packery(grid, {
        columnWidth: 100,
        itemSelector: '.brick',
        gutter: 10
      });
      grid.observer = new MutationObserver(function(mutations) {
          packery.reloadItems();
          packery.layout();
      });
      grid.observer.observe(grid.querySelector('ul'), { childList: true });
    });
  }

  if (!grid && packery) {
    packery.destroy();
    packery = null;
  }
}
