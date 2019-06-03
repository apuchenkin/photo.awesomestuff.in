// @ts-ignore
import sitemap from 'sitemap';
import fs from 'fs';
import * as R from 'ramda';

// @ts-ignore
import config from '../etc/config';
import serviceFactory from './service/factory';

interface URLData {
  url: string;
  changefreq: string;
  priority: number;
  langs: string[];
  img?: string;
}

const pages = config.locales.map(async (locale: Locale): Promise<URLData[]> => {
  const {
    categoryService,
    pageService,
  } = serviceFactory({
    locale,
    endpoint: process.env.API_ENDPOINT || `${config.hostname}${config.apiEndpoint}`,
  });

  const pages = await pageService.fetchPages();
  const categories = await categoryService.fetchCategories();

  const categories$ = await Promise.all(categories
    .reduce((acc, category) => {
      const chidlren = category.children || [];

      return [...acc, category, ...chidlren.map(child => ({ ...child, parent: category }))]
    }, [])
    .map(async category => {
    const photos = await categoryService.fetchPhotos(category);

    return {
      ...category,
      photos,
    }
  }));

  const pageLinks = pages.map(page => ({
    url: `/${page.alias}`,
    changefreq: 'monthly',
    priority: 1,
  }));

  const categoriesLinks = categories$.reduce((acc, category) => {
    const url = category.parent ? `/${category.parent.name}/${category.name}` : `/${category.name}`;

    const photoLinks = category.photos.map(photo => ({
      url: `${url}/photo/${photo.id}`,
      changefreq: 'monthly',
      priority: 0.5,
      img: `/static/photo/${photo.src}`,
    }));

    return [
      ...acc,
      { url, changefreq: 'monthly', priority: 0.8 },
      ...photoLinks,
    ];
  }, []);

  return [
    { url: '', changefreq: 'monthly', priority: 1 },
    ...pageLinks,
    ...categoriesLinks,
  ].map(page => ({ ...page, langs: [locale] }));
});

export default (async () => {
  const pages$ = await Promise.all(pages);
  const pageIndex = R.groupBy(R.prop('url'), R.unnest(pages$));

  const urls = R.values(R.mapObjIndexed((data, url) => {
    const langs = R.flatten(R.map(R.prop('langs'), data));
    const links = [
      { lang: 'x-default', url },
      ...langs.map(lang => ({ lang, url: `/${lang}${url}` })),
    ];

    return {
      url,
      ...R.head(data),
      langs,
      links,
    }
  }, pageIndex));

  const sitemap$ = sitemap.createSitemap({
    hostname: config.hostname,
    cacheTime: 600000, // 600 sec - cache purge period
    urls,
  });

  await fs.writeFileSync("dist/sitemap.xml", sitemap$.toString());
})();