import { ChildEntity, ManyToOne } from "typeorm";
import { Category } from './category';
import { Translation } from '../translation';

@ChildEntity()
export class CategoryTranslation extends Translation {

  @ManyToOne(() => Category, category => category.translations)
  category: Category;
}

export default CategoryTranslation;