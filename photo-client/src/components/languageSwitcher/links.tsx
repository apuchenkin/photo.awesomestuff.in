import React, { FunctionComponent } from 'react';
import { Helmet } from 'react-helmet';
import { ConfigContext } from '@app/context';

interface Props {
  langs: string[],
  url: string,
}

const Links: FunctionComponent<Props> = ({
  langs,
  url,
}) => {
  const { hostname } = React.useContext(ConfigContext);

  return (
    <Helmet>
      <link href={`${hostname}${url}`} rel="alternate" hrefLang="x-default" key="x-default" />
      {
        langs.map(lang => (
          <link rel="alternate" href={`${hostname}/${lang}${url}`} hrefLang={lang} key={lang} />
        ))
      }
    </Helmet>
  )
};


export default Links;
