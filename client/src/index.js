'use strict';

require('./index.html');
var Elm = require('./Main');
var css = require("!style!css!less!../assets/styles/main.less");

var Main = Elm.embed(Elm.Main, document.getElementById('main'), {
  localePort: navigator.language,
  timePort: Date.now()
});

Main.ports.meta.subscribe(metaUpdate);
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

// create an observer instance
var observer = new MutationObserver(function(mutations) {
  pckry.reloadItems();
  pckry.layout();
});

Main.ports.rs.subscribe(onTransition);

var pckry = null;
var config = { attributes: true, childList: true, characterData: true };

function onTransition(args) {
  var grid = document.querySelector('#gallery');
  // configuration of the observer:
  if (grid) {
    require.ensure([], function() {
      var Packery = require('packery');
      pckry = pckry || new Packery(grid, {
        columnWidth: 100,
        itemSelector: '.brick',
        gutter: 10
      });

      observer.observe(grid.querySelector('ul'), config);
    });
  } else {
    if (pckry) {
      pckry.destroy();
    }
    observer.disconnect();
    pckry = null;
  }
}
