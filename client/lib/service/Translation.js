import query from 'query-string';
import Service from './BaseService';

export default class TranslationService extends Service {

  baseUrl() {
    return `${super.baseUrl()}/translation`;
  }

  load(search) {
    return this.fetch(search ? `?${query.stringify(search)}` : '')
      .then(Service.respondJSON);
  }

  create(translation) {
    return this.fetch('', {
      method: 'POST',
      body: JSON.stringify(translation),
    }).then(Service.respondJSON);
  }

  delete(translation) {
    return this.fetch(`/${translation.id}`, {
      method: 'DELETE',
    });
  }

  update(translation, data) {
    return this.fetch(`/${translation.id}`, {
      method: 'PATCH',
      body: JSON.stringify(data),
    }).then(Service.respondJSON);
  }
}
