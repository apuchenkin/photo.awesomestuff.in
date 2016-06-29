'use strict';

require('./index.html');
require('./polyfill/scope.js');

var Elm = require('./Main');
var css = require("../assets/styles/main.less");
var fontello = require('../assets/fontello/css/fontello.css');
var Ps = require('perfect-scrollbar');
var pscss = require('perfect-scrollbar/dist/css/perfect-scrollbar.css');

var wrapper = document.body.querySelector('.wrapper');
while (wrapper.firstChild) {
    wrapper.removeChild(wrapper.firstChild);
}
var Main = Elm.Main.embed(wrapper, {
  locale: window.navigator.userLanguage || window.navigator.language,
  time: Date.now()
});

Main.ports.meta.subscribe(metaUpdate);
// Main.ports.photos.subscribe(onPhotosLoad);
Main.ports.transition.subscribe(onTransition);

var links = {};
var packery;

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

function onTransition(route) {
  var main = wrapper.querySelector(':scope > #main');
  var content = main.querySelector(':scope > .content');
  var gallery = content.querySelector(':scope > .gallery > ul');

  content.scrollTop = 0;
  Ps.initialize(content);
  if (!packery && gallery) {
    require.ensure([], function() {
      var Packery = require('packery');

      packery = new Packery(gallery, {
        columnWidth: 100,
        itemSelector: 'li',
        gutter: 10,
        initLayout: false
      });
      // debugger;
      packery.reloadItems();
      packery.layout();
    })
  } else {
    packery.delay = [];
    packery.reload = function() {
      packery.isLoading = true;
      packery.reloadItems();
      packery.layout();
    }
    if (gallery.children.length) {
      packery.reload();
    }

    packery.on("layoutComplete", function() {
      packery.isLoading = false;
      if (packery.delay.length) {
        var fn = packery.delay.pop();
        fn.apply(packery);
      }
    });

    packery.observer = new MutationObserver(function(mutations) {

        if (!packery.isLoading) {
          packery.reload();
        } else {
          packery.reload();
          packery.delay.push(packery.reload);
        }
    });
    packery.observer.observe(gallery, { childList: true });
  }
//);
  // }


  // }
  //
  // if (packery && !gallery) {
  //   packery.observer.disconnect();
  //   packery.destroy();
  //   packery = null;
  // }
  if (!gallery && packery) {
      debugger;
      packery.destroy();
      packery = null;
  }
}

function onPhotosLoad() {

    // clean up
    // content.scrollTop = 0;
    // Ps.destroy(content);

    var main = wrapper.querySelector(':scope > #main');
    var content = main.querySelector(':scope > .content');
    var gallery = content.querySelector(':scope > .gallery > ul');
    console.log(gallery.children.length);


}
