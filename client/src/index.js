'use strict';

require('./index.html');
require('./polyfill/scope.js');

var Elm = require('./Main');
var css = require("../assets/styles/main.less");
var fontello = require('../assets/fontello/css/fontello.css');
var Ps = require('perfect-scrollbar');
var pscss = require('perfect-scrollbar/dist/css/perfect-scrollbar.css');
var Packery = require('packery');

var wrapper = document.body.querySelector('.wrapper');
while (wrapper.firstChild) {
    wrapper.removeChild(wrapper.firstChild);
}
var Main = Elm.Main.embed(wrapper, {
  locale: window.navigator.userLanguage || window.navigator.language,
  time: Date.now()
});

Main.ports.meta.subscribe(metaUpdate);
Main.ports.photos.subscribe(onPhotosLoad);
Main.ports.transition.subscribe(onTransition);

var links = {};

function metaUpdate(meta) {
  document.title = meta.title;
  document.head.querySelector('meta[name=description]').content = meta.description;
  meta.links.map(function(data) {
    var link = links[data[0]] || document.head.appendChild(document.createElement('link'));
    link.href = data[1];
    link.rel = "alternate";
    link.hreflang = data[0];
    links[data[0]] = link;
  })
}

var packery;

function onTransition(route) {
  var main = wrapper.querySelector(':scope > #main');
  var content = main.querySelector(':scope > .content');
  var gallery = content.querySelector(':scope > .gallery > ul');

  content.scrollTop = 0;
  Ps.initialize(content);

  // clean up packery if gallery is hidden
  if (packery && !gallery) {
      packery.destroy();
      packery = null;
  }
}

function onPhotosLoad() {
    var main = wrapper.querySelector(':scope > #main');
    var content = main.querySelector(':scope > .content');
    var gallery = content.querySelector(':scope > .gallery > ul');

    if (!packery) {
      require.ensure([], function() {
        var Packery = require('packery');

        packery = new Packery(gallery, {
          columnWidth: 100,
          itemSelector: 'li',
          gutter: 10
        });
      })
    } else {
      setTimeout(function() {
        packery.reloadItems();
        packery.layout();
      });
    }
}
