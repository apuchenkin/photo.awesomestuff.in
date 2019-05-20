import path from 'path';
import IntlPolyfill from 'intl';
import { readFileSync } from 'fs';

Intl.NumberFormat = IntlPolyfill.NumberFormat;
Intl.DateTimeFormat = IntlPolyfill.DateTimeFormat;

const localeDataCache = new Map();
const getLocaleDataScript = (locale: string) => {
  if (!localeDataCache.has(locale)) {
    const localeDataFile = path.join(
      process.cwd(),
      'node_modules',
      'react-intl',
      'locale-data',
      `${locale}.js`,
    );

    const localeDataScript = readFileSync(localeDataFile, 'utf8');
    localeDataCache.set(locale, localeDataScript);
  }
  return localeDataCache.get(locale);
};

export default getLocaleDataScript;
