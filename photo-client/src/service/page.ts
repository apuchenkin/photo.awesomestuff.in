import ApiService from './api';

export default class PageService extends ApiService {

  fetchPages = async (): Promise<Page[]> => {
    return this.fetch('/page');
  }

  fetchPage = async (alias: string): Promise<Page> => {
    return this.fetch(`/page/${alias}`);
  }
}
