'use strict';

require('./index.html');
var Elm = require('./Main');
var css = require("../assets/styles/main.less");
var fontello = require('../assets/fontello/css/fontello.css');
var Ps = require('perfect-scrollbar');
var pscss = require('perfect-scrollbar/dist/css/perfect-scrollbar.css');


var main = document.body;
var Main = Elm.fullscreen(Elm.Main, {
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

Ps.initialize(document.querySelector('.content'));

var pckry = null;
var config = { attributes: true, childList: true, characterData: true };

function onTransition(args) {
  var mHeight = main.querySelector('header').offsetHeight;
  main.setAttribute("style", "padding-top: " + mHeight + "px;");
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
