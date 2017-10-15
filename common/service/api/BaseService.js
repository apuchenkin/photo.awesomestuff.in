import fetch from 'isomorphic-unfetch';

const defaults = {
  contentType: 'application/json',
};

const locales = {
  'ru': 'ru-RU',
  'en': 'en-US',
};

export default class BaseService {

  constructor(options = {}) {
    Object.assign(this, defaults, options);

    this.headers = {
      'Content-Type': this.contentType,
      'Accept-Language': this.locale && locales[this.locale],
      Authorization: this.token,
      Accept: this.contentType,
    };
  }

  static respondJSON(response) {
    return response.json().then(data => Object.assign(data, {
      __response: response,
    }));
  }

  baseUrl() {
    return this.apiEndpoint;
  }

  fetch(url, options = {}) {
    return fetch(`${this.baseUrl()}${url}`, Object.assign({
      headers: Object.keys(this.headers).reduce((acc, key) => (
        this.headers[key] ? Object.assign(acc, {
          [key]: this.headers[key],
        }) : acc), {}),
    }, options))
      .then(response => {
        if (!response.ok) {
          const error = new Error(response.statusText);
          error.response = response;
          throw error;
        }

        return response;
      });
  }
}
