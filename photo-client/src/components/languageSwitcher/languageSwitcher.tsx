import React, { FunctionComponent } from 'react';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Links from './links';
import Locale from './locale';
import style from './languageSwitcher.scss';
import { FoundContext, ConfigContext } from '@app/context';

interface Props {
  langs: string[];
}

const LanguageSwitcher: FunctionComponent<Props> = ({ langs }) => {
  const { locales } = React.useContext(ConfigContext);
  const { match } = React.useContext(FoundContext);
  const langs$ = langs || locales;
  const location = match.location;

  return (
    <div className={style.language}>
      {locales.map((locale: Locale) => (
        <Locale
          key={locale}
          location={location}
          locale={locale}
          disabled={!langs$.find(l => locale === l)}
        />
      ))}
      <Links langs={langs$} url={location.pathname} />
    </div>
    )
};

export default withStyles(style)(LanguageSwitcher);
