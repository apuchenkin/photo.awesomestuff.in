import React from 'react';
import { connect } from 'react-redux';
import { arrayOf, string } from 'prop-types';
import { Helmet } from 'react-helmet';

const Links = ({
  langs,
  hostname,
  url,
}) => (
  <Helmet>
    <link href={`${hostname}${url}`} rel="alternate" hrefLang="x-default" key="x-default" />
    {
      langs.map(lang => (
        <link rel="alternate" href={`${hostname}/${lang}${url}`} hrefLang={lang} />
      ))
    }
  </Helmet>
);

Links.propTypes = ({
  langs: arrayOf(string).isRequired,
  hostname: string.isRequired,
  url: string.isRequired,
});

export default connect(
  ({ runtime: { config } }) => ({
    hostname: config.hostname,
  }),
)(Links);
