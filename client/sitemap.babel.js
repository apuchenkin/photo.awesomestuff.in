/* eslint-disable import/no-extraneous-dependencies */
import sitemap from 'sitemap';
// import { Promise } from 'es6-promise';

import config from './src/etc/config.json';
import CategoryService from '../common/service/api/Category';
import PhotoService from '../common/service/api/Photo';
import PageService from '../common/service/api/Page';
import { getSrc } from '../common/service/photo';

const merge = urls => urls.reduce((acc, location) => {
  const exist = acc.find(a => a.url === location.url);
  if (exist) {
    exist.langs = exist.langs.concat(location.langs);
  }

  return exist ? acc : acc.concat([location]);
}, []);

const fetchAll = (object) => {
  const keys = Object.keys(object);

  return Promise
    .all(keys.map(k => object[k]))
    .then(data => keys.reduce((acc, k, idx) => Object.assign(acc, { [k]: data[idx] }), {}));
};

const createLinks = urls => urls.map(url =>
  Object.assign(url, { links: [{ lang: 'x-default', url: url.url }].concat(
      url.langs.map(lang => ({ lang, url: `/${lang}${url.url}` })),
    ),
  }),
);

const createSitemap = urls => sitemap.createSitemap({
  hostname: config.hostname,
  cacheTime: 600000, // 600 sec - cache purge period
  urls,
})
.toString();


const pages = config.locales.map((locale) => {
  const defaults = { locale, apiEndpoint: config.apiEndpoint };
  const categoryService = new CategoryService(defaults);
  const photoService = new PhotoService(defaults);
  const pageService = new PageService(defaults);

  return fetchAll({
    locale: Promise.resolve(locale),
    pages: pageService.fetchPages(),
    categories: categoryService.fetchCategories().then(categories =>
      Promise.all(categories.map(category =>
        photoService.fetchPhotos(category.id).then(photos =>
          Object.assign(category, { photos }),
        ),
      )),
    ),
  });
});

export default () => Promise
  .all(pages)
  .then(langs => langs.reduce((links, lang) => {
    const rootLinks = [{ url: '', changefreq: 'monthly', priority: 1 }];
    const pageLinks = lang.pages.map(page => ({
      url: `/${page.alias}`,
      changefreq: 'monthly',
      priority: 1,
    }));
    const categoriesLinks = lang.categories.reduce((acc, category) => {
      const url = category.parent ? `/${category.parent.name}/${category.name}` : `/${category.name}`;
      const photoLinks = category.photos.map(photo => ({
        url: `${url}/photo/${photo.id}`,
        changefreq: 'monthly',
        priority: 0.5,
        img: [config.staticEndpoint, getSrc(photo.src, config.photo.width, config.photo.height)].join('/'),
      }));

      return acc.concat([{
        url,
        changefreq: 'monthly',
        priority: 0.8,
      }], photoLinks);
    }, []);

    return links.concat(
      [].concat(
        rootLinks,
        pageLinks,
        categoriesLinks,
      ).map(url => Object.assign(url, { langs: [lang.locale] }),
    ));
  }, []))
  .then(merge)
  .then(createLinks)
  .then(createSitemap);
