/* eslint-disable import/no-extraneous-dependencies */
import sitemap from 'sitemap';
import { Promise } from 'es6-promise';

import config from './src/etc/config.json';
import CategoryService from './lib/service/Category';
import PhotoService from './lib/service/Photo';
import PageService from './lib/service/Page';
import utils from './src/lib/utils';
import { getSrc } from './lib/util/photo';

const merge = urls => urls.reduce((acc, location) => {
  const exist = acc.find(a => a.url === location.url);
  if (exist) {
    exist.langs = exist.langs.concat(location.langs);
  }

  return exist ? acc : acc.concat([location]);
}, []);


const createLinks = urls => urls.map(url =>
  Object.assign(url, { links: [{ lang: 'x-default', url: url.url }].concat(
      url.langs.map(lang => ({ lang, url: `/${lang}${url.url}` }))
    ),
   })
);

const createSitemap = urls => sitemap.createSitemap({
  hostname: config.hostname,
  cacheTime: 600000, // 600 sec - cache purge period
  urls,
})
.toString();


const pages = config.locales.map((locale) => {
  const
    defaults = { locale, apiEndpoint: config.apiEndpoint },
    categoryService = new CategoryService(defaults),
    photoService = new PhotoService(defaults),
    pageService = new PageService(defaults);

  return utils.fetchAll({
    locale: Promise.resolve(locale),
    pages: pageService.fetchPages(),
    categories: categoryService.fetchCategories().then(categories =>
      Promise.all(categories.map(category =>
        photoService.fetchPhotos(category.id).then(photos =>
          Object.assign(category, { photos })
        )
      ))
    ),
  });
});

export default () => Promise
  .all(pages)
  .then(langs => langs.reduce((links, lang) => {
    const
      rootLinks = [{ url: '', changefreq: 'monthly', priority: 1 }],
      pageLinks = lang.pages.map(page => ({
        url: `/${page.alias}`,
        changefreq: 'monthly',
        priority: 1,
      })),
      categoriesLinks = lang.categories.reduce((acc, category) => {
        const
          url = category.parent ? `/${category.parent.name}/${category.name}` : `/${category.name}`,
          photoLinks = category.photos.map(photo => ({
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
      ).map(url => Object.assign(url, { langs: [lang.locale] })
    ));
  }, []))
  .then(merge)
  .then(createLinks)
  .then(createSitemap);
