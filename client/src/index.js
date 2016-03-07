'use strict';

require('./index.html');
var Elm = require('./Main');
var css = require("../assets/styles/main.less");
var fontello = require('../assets/fontello/css/fontello.css');
var Ps = require('perfect-scrollbar');
var pscss = require('perfect-scrollbar/dist/css/perfect-scrollbar.css');

var wrapper = document.body.querySelector('.wrapper');
var Main = Elm.embed(Elm.Main, wrapper, {
  localePort: navigator.language,
  timePort: Date.now()
});

Main.ports.meta.subscribe(metaUpdate);
Main.ports.rs.subscribe(onTransition);

var main = wrapper.querySelector(':scope > #main');
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

var content = main.querySelector(':scope > .content');
Ps.initialize(content);

var headerElm = content.querySelector(':scope > header');
var headerObserver = new MutationObserver(function(mutations) {
  main.setAttribute("style", "padding-top: " + headerElm.offsetHeight + "px;");
});
main.setAttribute("style", "padding-top: " + headerElm.offsetHeight + "px;");
headerObserver.observe(headerElm, {childList: true});

var packery;

function onTransition() {
    content.scrollTop = 0;
    Ps.update(content);

    var gallery = content.querySelector(':scope > .gallery > ul');

    if (gallery && !packery) {
      require.ensure([], function() {
        var Packery = require('packery');
        packery = new Packery(gallery, {
          columnWidth: 100,
          itemSelector: 'li',
          gutter: 10
        });
        packery.observer = new MutationObserver(function(mutations) {
            packery.reloadItems();
            packery.layout();
        });
        packery.observer.observe(gallery, { childList: true });
      });
    } else if (!gallery && packery) {
        packery.observer.disconnect();
        packery.destroy();
        packery = null;
    }
}
