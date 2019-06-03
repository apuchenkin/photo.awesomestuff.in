import ApiService from './api';

export default class CategoryService extends ApiService {
  fetchCategories = (): Promise<Category[]> => {
    return this.fetch('/category');
  }

  fetchCategory = (name: string): Promise<Category> => {
    return this.fetch(`/category/${name}`);
  }

  fetchPhotos = (category: Category): Promise<Photo[]> => {
    return this.fetch(`/category/${category.name}/photo`);
  }
}
