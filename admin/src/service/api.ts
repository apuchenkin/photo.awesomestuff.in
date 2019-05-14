interface Options {
  endpoint: string;
}

export default class BaseService {
  readonly endpoint: string;

  constructor({
    endpoint,
  }: Options) {
    this.endpoint = endpoint;
  }

  fetch(url: string, options: RequestInit = {}) {
    return fetch(`${this.endpoint}${url}`, {
      ...options,
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        ...options.headers,
      },
    }).then(response => response.json());
  }
}
