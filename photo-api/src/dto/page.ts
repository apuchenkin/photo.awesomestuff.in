import * as R from 'ramda';

import { Language } from "@app/entity/translation";
import { Page } from "@app/entity";

export interface PageDTO {
  alias: string;
  title: string;
  content?: string;
  langs?: Language;
}

export const pageDTO = (page: Page): PageDTO => {
  const translations = R.map(
    R.prop('value'),
    R.indexBy(R.prop('field'), page.translations),
  );

  const langs = page.languages && R.uniq(R.map(R.prop('language'), page.languages));

  return {
    alias: page.alias,
    title: translations.title,
    content: translations.content,
    langs,
  }
}