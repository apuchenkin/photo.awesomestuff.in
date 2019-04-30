import { RequestHandler } from "express";
import acceptLanguage from 'accept-language';
import { Language } from '@app/entity/translation';

acceptLanguage.languages(['en-US', 'ru-RU']);

const localeHandler: RequestHandler = (req, res, next) => {
  const locale = acceptLanguage.get(req.get('accept-language'));

  req.app.locals.locale = {
    'en-US': Language.EN,
    'ru-RU': Language.RU,
  }[locale] || Language.EN;

  next();
};

export default localeHandler;