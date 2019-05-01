import { Connection } from "typeorm";
import { Language } from "@app/entity/translation";
import { Category, CategoryTranslation } from "@app/entity";

interface TranslationDTO {
  field: string;
  value: string;
  language: Language;
}

interface CategoryDTO {
  name: string;
  hidden: boolean;
  date: string;
  translations: TranslationDTO[];
  parent?: CategoryDTO;
}

export default (connection: Connection) => async (categories: CategoryDTO[]) => {
  console.log("Start migrating categories...");

  const records = categories.map(categoryDTO => {
    console.info(`Persist category: ${categoryDTO.name}`);

    const category = new Category();

    category.name = categoryDTO.name;
    category.hidden = categoryDTO.hidden;
    category.date = categoryDTO.date && new Date(categoryDTO.date);

    category.translations = categoryDTO.translations.map(translationDTO => {
      const translation = new CategoryTranslation();

      translation.field = translationDTO.field;
      translation.value = translationDTO.value;
      translation.language = translationDTO.language;
      translation.category = category;

      return translation;
    })

    return category;
  })

  await connection.manager.save(records);

  const relations = await Promise.all(categories.map(async categoryDTO => {
    const category = await connection.manager.findOne(Category, { name: categoryDTO.name });

    if (category && categoryDTO.parent) {
      const parent = await connection.manager.findOne(Category, { name: categoryDTO.parent.name });
      category.parent = parent;
    }

    return category;
  }))

  await connection.manager.save(relations);
}