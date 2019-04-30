import { ChildEntity, ManyToOne } from "typeorm";
import { Page } from './page';
import { Translation } from '../translation';

@ChildEntity()
export class PageTranslation extends Translation {

  @ManyToOne(type => Page, page => page.translations)
  page: Page;
}

export default PageTranslation;