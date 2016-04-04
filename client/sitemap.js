'use strict';

var
  _ = require('lodash')
, sm = require('sitemap')
, fs = require('fs')
, http = require('http')
, Q = require('q')
, urls = []
, promises = []
, hostname = 'http://photo.awesomestuff.in'
, apiEndpoint = 'api/v1'
, w = 1200
, h = 900
;

var deferred = Q.defer();
var remapPhoto = function (photo) {
  return [hostname, apiEndpoint, 'hs/photo', photo.id, w, h, photo.src.split('/').pop()].join('/');
};

var genLinks = function(url) {
  return [
    {lang: 'x-default', url: url},
    {lang: 'ru', url: '/ru' + url},
    {lang: 'en', url: '/en' + url}
  ];
};

urls.push({ url: '/', changefreq: 'monthly', priority: 1, links: genLinks('/')});
urls.push({ url: '/about',  changefreq: 'monthly', priority: 0.5, links: genLinks('/about')});
urls.push({ url: '/contacts',  changefreq: 'monthly', priority: 0.5, links: genLinks('/contacts')});

http.get(
  {
    host: 'proxy02.merann.ru',
    port: 8080,
    path: _.filter([hostname, apiEndpoint, 'category']).join('/')
  }, function(response) {
  console.log('Got response: ' + response.statusCode);
  var body = '';
  response.on('data', function(d) {
    body += d;
  });
  response.on('end', function() {
    // Data reception is done, do whatever with it!
    var categories = JSON.parse(body),
        groups = _.groupBy(categories, 'parent');

    _.map(groups, function (group, key) {
      _.map(group, function (c) {
        var url;
        if (key === 'null') {
          url = '/' + c.name;
          urls.push({ url: url,  changefreq: 'weekly', priority: 0.8, links: genLinks(url)});
        } else {
          var parent = _.find(categories, {id: +key});
          url = '/' + parent.name + '/' + c.name;
          urls.push({ url: url,  changefreq: 'weekly', priority: 0.6, links: genLinks(url)});
        }

        var d = Q.defer();
        promises.push(d.promise);

        http.get(
          {
            host: 'proxy02.merann.ru',
            port: 8080,
            path: _.filter([hostname, apiEndpoint, 'category', c.id, 'photo']).join('/')
          },
          function(response) {
          console.log('Got response: ' + response.statusCode);
          var body = '';
          response.on('data', function(d) {
            body += d;
          });
          response.on('end', function() {
            // Data reception is done, do whatever with it!
            var photos = JSON.parse(body);
            photos.map(function(p) {
              urls.push({
                url: url + '/photo/' + p.id,
                changefreq: 'monthly',
                priority: 0.4,
                img: remapPhoto(p),
                links: genLinks(url + '/photo/' + p.id)
              });
            });
            d.resolve();
          });
        }).on('error', function(e) {
          console.log('Got error: ' + e.message);
          d.reject(e.message);
        });
      });
    });
    Q.all(promises).then(function() {
      deferred.resolve();
    });
  });
}).on('error', function(e) {
  console.log('Got error: ' + e.message);
  deferred.reject(e.message);
});

deferred.promise.then(function(){
  var sitemap = sm.createSitemap ({
    hostname: hostname,
    cacheTime: 600000,        // 600 sec - cache purge period
    urls: urls
  });

  fs.writeFileSync('./dist/sitemap.xml', sitemap.toString());
});
