import Service from './BaseService';

export default class PageService extends Service {
  baseUrl() {
    return `${super.baseUrl()}/page`;
  }

  fetchPages() {
    return this.fetch('')
    .then(Service.respondJSON);
  }

  fetchPage(page) {
    return this.fetch(`/${page.alias}`)
    .then(Service.respondJSON);
  }
}
