import CategoryService from 'common/service/api/Category';
import PageService from 'common/service/api/Page';
import PhotoService from 'common/service/api/Photo';

export default function createServices({ locale, apiEndpoint }) {
  const config = { locale, apiEndpoint };

  return {
    categoryService: new CategoryService(config),
    pageService: new PageService(config),
    photoService: new PhotoService(config),
  };
}
