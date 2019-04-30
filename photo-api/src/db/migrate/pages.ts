import { Connection } from "typeorm";
import { Page, PageTranslation } from '@app/entity';
import { Language } from "@app/entity/translation";

interface TranslationDTO {
  field: string;
  value: string;
  language: Language;
}

interface PageDTO {
  alias: string;
  hidden: boolean;
  translations: TranslationDTO[];
}

export default (connection: Connection) => async (pages: PageDTO[]) => {
  console.log("Start migrating pages...");

  const records = pages.map(pageDTO => {
    console.info(`Persist page: ${pageDTO.alias}`);

    const page = new Page();

    page.alias = pageDTO.alias;
    page.hidden = pageDTO.hidden;
    page.translations = pageDTO.translations.map(translationDTO => {
      const translation = new PageTranslation();

      translation.field = translationDTO.field;
      translation.value = translationDTO.value;
      translation.language = translationDTO.language;
      translation.page = page;

      return translation;
    })

    return page;
  })

  await connection.manager.save(records);

  return records;
}