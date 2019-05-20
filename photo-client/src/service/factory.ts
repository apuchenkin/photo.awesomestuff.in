import PageService from './page';
import CategoryService from './category';
import PhotoService from './photo';

interface Config {
  locale: Locale;
  endpoint: string;
}

export interface Services {
  categoryService: CategoryService;
  pageService: PageService;
  photoService: PhotoService;
}

export default function createServices(config: Config): Services {
  return {
    categoryService: new CategoryService(config),
    pageService: new PageService(config),
    photoService: new PhotoService(config),
  };
}
