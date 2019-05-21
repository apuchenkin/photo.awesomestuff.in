import React, { FunctionComponent } from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import Link from 'found/lib/Link';
import LanguageSwitcher from '@app/components/languageSwitcher';
import { ConfigContext, PageContext } from '@app/context';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import style from './footer.scss';

const messages = defineMessages({
  footer: {
    id: 'footer',
    defaultMessage: '2016, Artem Puchenkin',
  },
});

interface Props {
  langs: Locale[];
}

const Footer: FunctionComponent<Props> = ({ langs }) => {
  const { title } = React.useContext(ConfigContext);
  const { getPage } = React.useContext(PageContext);

  const aboutPage = getPage('about');
  const contactsPage = getPage('contacts');

  return (
    <footer className={style.footer}>
      <Link to="/" >{title}</Link> | &copy; <FormattedMessage {...messages.footer} />
      {aboutPage && aboutPage.title && [' | ', <Link to="/about" key="page.about">{aboutPage.title}</Link>]}
      {contactsPage && contactsPage.title && [' | ', <Link to="/contacts" key="page.contacts">{contactsPage.title}</Link>]}
      <LanguageSwitcher langs={langs} />
    </footer>
  );
};

export default withStyles(style)(Footer);
