import PageService from './page';
import CategoryService from './category';
import PhotoService from './photo';
import { Options } from './api';


export interface Services {
  categoryService: CategoryService;
  pageService: PageService;
  photoService: PhotoService;
}

export default function createServices(config: Options): Services {
  return {
    categoryService: new CategoryService(config),
    pageService: new PageService(config),
    photoService: new PhotoService(config),
  };
}
