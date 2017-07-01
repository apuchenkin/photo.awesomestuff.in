import React from 'react';
import { string, arrayOf, shape } from 'prop-types';
import { connect } from 'react-redux';
import { defineMessages, FormattedMessage } from 'react-intl';
// import { locationShape } from 'found/lib/PropTypes';
import Link from 'found/lib/Link';

import LanguageSwitcher from './common/LanguageSwitcher';

const messages = defineMessages({
  footer: {
    id: 'footer',
    defaultMessage: '2016, Artem Puchenkin',
  },
});

const Footer = ({ title, pages }) => {
  const aboutPage = pages.find(p => p.alias === 'about');
  const contactsPage = pages.find(p => p.alias === 'contacts');

  return (
    <footer>
      <Link to="/" >{title}</Link> | &copy; <FormattedMessage {...messages.footer} />
      {aboutPage && contactsPage.title && [' | ', <Link to="/about" key="page.about">{aboutPage.title}</Link>]}
      {contactsPage && contactsPage.title && [' | ', <Link to="/contacts" key="page.contacts">{contactsPage.title}</Link>]}
      <LanguageSwitcher />
    </footer>
  );
};

Footer.propTypes = {
  title: string.isRequired,
  pages: arrayOf(shape({
    alias: string.isRequired,
    title: string,
  })).isRequired,
};

export default connect(
  ({ runtime: { config }, page: { pages } }) => ({
    title: config.title,
    pages,
  }),
)(Footer);
