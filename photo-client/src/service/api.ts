import 'isomorphic-unfetch';

const locales = {
  'ru': 'ru-RU',
  'en': 'en-US',
};

type Locale = 'ru' | 'en';

interface Options {
  locale: Locale;
  endpoint: string;
}

export default class BaseService {

  readonly accept = 'application/json';
  readonly contentType = 'application/json';
  readonly locale: Locale;
  readonly endpoint: string;

  constructor({
    locale = 'ru',
    endpoint,
  }: Options) {
    this.locale = locale;
    this.endpoint = endpoint;
  }

  fetch(url: string, options: RequestInit = {}) {
    return fetch(`${this.endpoint}${url}`, {
      ...options,
      headers: {
        'Accept': this.accept,
        'Content-Type': this.contentType,
        'Accept-Language': this.locale && locales[this.locale],
        ...options.headers,
      },
    }).then(response => response.json());
  }
}
