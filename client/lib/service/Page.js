import Service from './BaseService';

export default class PageService extends Service {
  baseUrl() {
    return `${super.baseUrl()}/page`;
  }

  fetchPages() {
    return this.fetch('')
    .then(Service.respondJSON);
  }

  fetchPage(pageId) {
    return this.fetch(`/${pageId}`)
    .then(Service.respondJSON);
  }
}
