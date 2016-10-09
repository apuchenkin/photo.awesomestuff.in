import config from '../config/config.json';

const defaults = {
  locale: config.fallbackLocale,
  contentType: 'application/json',
};

export default class BaseService {

  respondJSON(response) {
    if (!response.ok) {
      const error = new Error(response.statusText);
      error.response = response;
      throw error;
    }

    return response.json();
  }

  baseUrl() {
    return config.apiEndpoint;
  }

  constructor(options = {}) {
    Object.assign(this, defaults, options);

    this.headers = new Headers({
      'Content-Type': this.contentType,
      'Accept-Language': this.locale,
      Authorization: this.token,
      Accept: this.contentType,
    });
  }
}
