export interface Options {
  endpoint: string;
  token: string;
}

export default class BaseService {
  readonly endpoint: string;
  readonly token: string;

  constructor({
    endpoint,
    token,
  }: Options) {
    this.endpoint = endpoint;
    this.token = token;
  }

  fetch(url: string, options: RequestInit = {}) {
    return fetch(`${this.endpoint}${url}`, {
      ...options,
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': `Basic ${this.token}`,
        ...options.headers,
      },
    })
    .then(async response => {
      if (!response.ok) {
        const error = await response.json();
        throw new Error(error);
      }

      if (response.status === 204 || response.status === 304) {
        return response;
      }

      return response.json();
    })
  }
}
