const defaults = {
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
    return this.apiEndpoint;
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
