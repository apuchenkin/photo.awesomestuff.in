/* eslint-disable import/no-extraneous-dependencies */
import sitemap from 'sitemap';
import { Promise } from 'es6-promise';

import config from './src/config/config';
import CategoryService from './src/service/Category';
import PhotoService from './src/service/Photo';
import PageService from './src/service/Page';
import utils from './src/lib/utils';

const getSrc = photo => PhotoService.getSrc(photo.src, {
  width: config.photo.width,
  height: config.photo.height,
});

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
    categoryService = new CategoryService({ locale }),
    photoService = new PhotoService({ locale }),
    pageService = new PageService({ locale });

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
            img: getSrc(photo), //TODO: add caption
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
