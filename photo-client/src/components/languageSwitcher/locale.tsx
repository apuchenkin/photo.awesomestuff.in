import React, { FunctionComponent } from 'react';
import { defineMessages, injectIntl, InjectedIntl } from 'react-intl';

const messages = defineMessages({
  not_available: {
    id: 'locale.not_available',
    defaultMessage: 'Current page content is not available in language {lang} yet',
  },
});

interface Props {
  location: Location,
  locale: string,
  disabled: boolean,
  intl: InjectedIntl,
};

const Locale: FunctionComponent<Props> = ({ location, locale, intl, disabled }) => {
  const localeMsg = intl.formatMessage({ id: locale, defaultMessage: locale });
  const title = intl.formatMessage(messages.not_available, { lang: localeMsg });

  return disabled
    ? <span title={title}>{localeMsg}</span>
    : <a href={`/${locale}${location.pathname}`} hrefLang={locale} >{localeMsg}</a>;
};

export default injectIntl(Locale);
