import ApiService from './api';

const base = `/category`;

export default class CategoryService extends ApiService {
  fetchCategories = (): Promise<Category[]> => {
    return this.fetch(base);
  }

  fetchCategory = (name: string): Promise<Category> => {
    return this.fetch(`${base}/${name}`);
  }

  fetchPhotos = (category: Category): Promise<Photo[]> => {
    return this.fetch(`${base}/${category.name}/photo`);
  }

  create = (category: Category): Promise<Category> => {
    return this.fetch(base, {
      method: 'POST',
      body: JSON.stringify(category),
    });
  }

  update = (category: Category): Promise<Category> => {
    return this.fetch(`${base}/${category.name}`, {
      method: 'PUT',
      body: JSON.stringify(category),
    });
  }

  delete = (category: Category): Promise<void> => {
    return this.fetch(`${base}/${category.name}`, {
      method: 'DELETE',
    });
  }

  linkPhotos = (category: Category, photos: Photo[]) => {
    return this.fetch(`${base}/${category.name}/photo`, {
      method: 'LINK',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }

  unlinkPhotos = (category: Category, photos: Photo[]) => {
    return this.fetch(`${base}/${category.name}/photo`, {
      method: 'UNLINK',
      body: JSON.stringify(photos.map(p => p.id)),
    });
  }
}
