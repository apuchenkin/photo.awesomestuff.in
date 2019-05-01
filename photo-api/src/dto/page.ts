import * as R from 'ramda';
import { Page } from "@app/entity";

export interface PageDTO {
  alias: string;
  title: string;
  content?: string;
}

export const pageDTO = (page: Page): PageDTO => {
  const translations = R.map(
    R.prop('value'),
    R.indexBy(R.prop('field'), page.translations),
  );

  return {
    alias: page.alias,
    title: translations.title,
    content: translations.content,
  }
}